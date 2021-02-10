{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
--
-- Modern revision of the tool
--
module P2021.R32 where

import Parser.Parser
import Data.Data
import Data.Either
import P2021.General
import qualified Data.Set as S
import Data.List
import Data.Matrix
import Data.Maybe
import qualified GVSpec.GVSpec as GV

import qualified Debug.Trace as DT

--
-- BoGL Stuff
--
import P2021.Bogl_Specifics

import Language.Syntax (Game)
import Text.Parsec.Pos
import qualified Data.Set as DS
import qualified DB.BoglDB as BDB
import ConceptGraph.GraphToConceptGraph (graph_to_concept_graph)
import ConceptGraph.Conceptual (graphCG)

-- terms are names in this context, both terminal & nonterminal
type Term = String

-- Object with attributes
type Object = String

-- attribute of an object
type Attribute = String
type Intent = Attribute

-- Representation of a singular form of implication
-- Many Ands -> Many Ands
-- A^B^C -> D^E
-- States "If an object has attributes A^B^C, then it has attributes D^E"
type Implication = ([Attribute],[Attribute])

-- A label for a formal concept corresponds
-- the objects & attributes that map directly to this closure
-- i.e. the single objects & attributes (sets of 1) that map to this
type Label = ([Object],[Attribute])

-- Pair of sets, (A subset G, B subset M)
-- A is the extent: set of all objs that have all the attributes in B
  -- For all 'a' in G. 'a' in A iff (For all 'b' in B. b(a))
-- B is the intent: set of all attributes that match to all objects in A
  -- For all 'b' in M. 'b' in B iff (For all 'a' in A. b(a))
-- Also has a name, for easy id
data FormalConcept = FormalConcept (Label,[Object],[Attribute])
  deriving (Eq,Show)

conceptName :: FormalConcept -> Label
conceptName (FormalConcept (n,_,_)) = n

conceptIntent :: FormalConcept -> [Attribute]
conceptIntent (FormalConcept (_,_,m)) = m

conceptExtent :: FormalConcept -> [Object]
conceptExtent (FormalConcept (_,g,_)) = g

instance Ord FormalConcept where
  (<=) a b = let r = compareFormalConceptsByExtents a b in r /= GT
  (<) a b = let r = compareFormalConceptsByExtents a b in r == LT
  (>=) a b = let r = compareFormalConceptsByExtents a b in r /= LT
  (>) a b = let r = compareFormalConceptsByExtents a b in r == GT
  compare a b = compareFormalConceptsByExtents a b

instance GV.ShowGV FormalConcept where
  showGV (FormalConcept (([],[]),_,_)) = ""
  showGV (FormalConcept ((g,[]),_,_)) = join "," g ++ " :: "
  showGV (FormalConcept (([],m),_,_)) = " :: " ++ join "," m
  showGV (FormalConcept ((g,m),_,_)) = join "," g ++ " :: " ++ join "," m

-- formal context (G,M,I)
-- G = Set of all objects (programs)
-- M = Set of all attributes (terms)
-- I = Subset of (G X M), Relation that links elements of G to elements of M
type FormalContext = ([Object],[Attribute],[(Object,Attribute)])

fcM :: FormalContext -> [Attribute]
fcM (_,m,_) = m

contextExtent :: FormalContext -> [Object]
contextExtent (g,_,_) = g

contextIntent :: FormalContext -> [Attribute]
contextIntent (_,m,_) = m

-- Ordering for Formal Concepts
data OrderBy =
  OrderByIntents |
  OrderByExtents

data ConceptsBy =
  ConceptsByIntents |
  ConceptsByExtents |
  AllConcepts

type ParsingFunction a = ([ConcreteProgram] -> [(String, a)])

type ConceptMapping b = (String -> Maybe b)

type KnownPrograms = [ConcreteProgram]
type GoalPrograms  = [ConcreteProgram]
type CoursePrograms= [ConcreteProgram]

-- Configuration for performing Formal Concept Analysis
data FCA a b = FCA OrderBy ConceptsBy (ParsingFunction a) (ConceptMapping b) KnownPrograms GoalPrograms CoursePrograms

type Lattice a = ([a],[(a,a)])
type ConceptLattice = ([FormalConcept],[(FormalConcept,FormalConcept)])

getAllNodes :: ConceptLattice -> [FormalConcept]
getAllNodes (nodes,_) = nodes

-- Return the lower neighbors of this node in the concept lattice, if any
getLowerNeighbors :: ConceptLattice -> FormalConcept -> [FormalConcept]
getLowerNeighbors (_,edges) fc = map snd $ filter (\(i,o) -> i == fc) edges

-- Return the upper neighbors of this node in the concept lattice, if any
getUpperNeighbors :: ConceptLattice -> FormalConcept -> [FormalConcept]
getUpperNeighbors (_,edges) fc = map fst $ filter (\(i,o) -> o == fc) edges

-- returns supremum of the concept lattice (Top most node, most general, concept of all objects)
getJoin :: ConceptLattice -> FormalConcept
getJoin (nodes,_) = head $ reverse $ sort nodes

-- returns the infimum of the concept lattice (Bottom most node, most specific, concept of all attributes)
getMeet :: ConceptLattice -> FormalConcept
getMeet (nodes,_) = head $ sort nodes

mapL :: (a -> b) -> Lattice a -> Lattice b
mapL f (nodes,edges) = (map f nodes, map (\(a,b) -> (f a, f b)) edges)

graphConceptLattice :: String -> ConceptLattice -> IO ()
graphConceptLattice name cl = GV.writeLattice name cl

-- updates a single concept in both the nodes & edges portion and returns the updated lattice
-- focuses on updating name only
updateInLattice :: ConceptLattice -> FormalConcept -> ConceptLattice
updateInLattice (nodes,edges) fc@(FormalConcept (_,a,b)) = let n = map maybeReplace nodes in
                                                           let e = map (\(f1,f2) -> (maybeReplace f1, maybeReplace f2)) edges in
                                                           (n,e) where
                                                             maybeReplace :: FormalConcept -> FormalConcept
                                                             maybeReplace fc2@(FormalConcept (_,a2,b2)) = if a == a2 && b == b2 then fc else fc2

data Wrapper = Wrap String

instance Show Wrapper where
  show (Wrap s) = s

asIsConceptMapping :: String -> Maybe Wrapper
asIsConceptMapping s = Just (Wrap s)

-- run the analysis, taking an FCA analysis instance
-- Takes known programs, goal programs, and course programs (programs available in the course)
r32 :: (Data a, Show a, Show b) => FCA a b -> IO ()
r32 (FCA ob cb programParser conceptMapping kps gps cps) = do
  -- parse the programs that are understood
  let parsedKPS = programParser kps
  -- parse the goal programs that are desired to be understood
  let parsedGPS = programParser gps
  -- parse course programs that are available as extents
  let parsedCPS = programParser cps

  -- extract terms for the known list of programs, preserving names
  let knownIntents = map (astToTerms conceptMapping) parsedKPS
  -- extract terms for the goal list of programs, preserving names
  let goalIntents = map (astToTerms conceptMapping) parsedGPS
  -- extract terms for the course list of programs, preserving names
  let courseIntents = map (astToTerms conceptMapping) parsedCPS

  -- produce total context
  let totalContext = mkFormalContext (uniqueInSameOrder' $ knownIntents ++ goalIntents ++ courseIntents)

  -- TESTING TO FILTER OUT DUPLICATE INTENTS
  --let bbb = uniqueInSameOrder'' $ knownIntents ++ goalIntents ++ courseIntents
  --putStrLn $ show $ length bbb

  -- produce total concepts from total context
  let totalConcepts' = getConceptsFromContext cb totalContext
  -- Apply Bounding concepts to get
  let boundingConcepts = mkFormalConceptsFromSubExtents totalContext ((map fst knownIntents) ++ (map fst goalIntents))
  let totalConcepts = if length boundingConcepts > 0 then filter (\x -> x >= (minimum boundingConcepts) && x <= (maximum boundingConcepts)) totalConcepts' else totalConcepts'

  putStrLn $ "Num Course Progs: " ++ (show $ length cps)
  putStrLn $ "Num Course Objects: " ++ (show $ length ((\(x,_,_) -> x) totalContext))
  putStrLn $ "Num Course attributes: " ++ (show $ length ((\(_,x,_) -> x) totalContext))
  putStrLn $ "Num Formal Concepts (Program,Attribute set pairs): " ++ (show $ length totalConcepts)
  --putStrLn $ showConcepts (sortBy (getConceptOrdering ob) totalConcepts)


  -- create Formal Concept of all intents
  let m' = FormalConcept (([],[]), [], (\(_,m,_) -> m) totalContext)
  -- create Formal Concept of all extents
  let g' = FormalConcept (([],[]), (\(a,_,_) -> a) totalContext, [])
  -- Add the Formal Concept of all Extents ONLY if a program does not exist that captures this notion
  let l1 = if doesConceptExtentAlreadyExist g' totalConcepts then totalConcepts else totalConcepts ++ [g']
  -- Add the Formal Concept of all Intents ONLY if a program does not exist that captures this notion
  let l2 = if doesConceptIntentAlreadyExist m' l1 then l1 else l1 ++ [m']
  --  add & filter out to avoid dups
  let sorted = reverse $ sort $ l2

  let conceptLattice = mkConceptLattice sorted


  --let implications = filterCommonPremise (conceptIntent $ getJoin conceptLattice) $ removeConflictingImplications $ deriveImplications (fcM totalContext) $ getTT totalContext
  let implications = filterCommonPremise (conceptIntent $ getJoin conceptLattice) $ deriveImplications (fcM totalContext) $ getTT totalContext
  --putStrLn $ "Num implications: " ++ (show $ length implications)
  --qq <- stringImplications $ sort $ implications

  --let prettyLattice = mapL conceptName conceptLattice
  graphConceptLattice "R32_Test_1" conceptLattice

  -- TODO full detailed formal context as a matrix
  let (pm,mm,matt) = mkFormalConceptMatrix totalContext
  putStrLn $ showMatLegend pm
  putStrLn $ showMatLegend mm
  --putStrLn $ prettyMatrix matt
  let smallMat = mkFormalConceptMatrixSmall totalContext
  putStrLn $ prettyMatrix smallMat
  putStrLn "* Exported to CSV"
  exportCSV totalContext smallMat

convertToCSV :: [[String]] -> String
convertToCSV ls = join "\n" $ map (\q -> join "," (map show q)) ls

addNames :: [Object] -> [[String]] -> [[String]]
addNames [] [] = []
addNames (x:xs) (y:ys) = (x : y) : addNames xs ys

exportCSV :: FormalContext -> Matrix Mark -> IO ()
exportCSV (g,m,i) mm = do
  let lsts = map (\q -> map show q) (toLists mm)
  let addedNames = addNames g lsts
  --let gm = map (\x -> [x]) g
  --let lsts2 = (concat . Data.List.transpose) [gm,lsts]
  writeFile ("export.csv") $ convertToCSV (("":m) : addedNames)

-- determine whether a concept extent already exists
doesConceptExtentAlreadyExist :: FormalConcept -> [FormalConcept] -> Bool
doesConceptExtentAlreadyExist (FormalConcept (_,extent,_)) ls = any (\(FormalConcept (_,g,_)) -> S.fromList g == S.fromList extent) ls

-- determine whether a concept intent already exists
doesConceptIntentAlreadyExist :: FormalConcept -> [FormalConcept] -> Bool
doesConceptIntentAlreadyExist (FormalConcept (_,_,intent)) ls = any (\(FormalConcept (_,_,m)) -> S.fromList m == S.fromList intent) ls

getConceptOrdering :: OrderBy -> (FormalConcept -> FormalConcept -> Ordering)
getConceptOrdering OrderByIntents = compareFormalConceptsByIntents
getConceptOrdering OrderByExtents = compareFormalConceptsByExtents
--getConceptOrdering OrderByAll = undefined -- since this is not a bool how do we order these?????

showConcepts :: [FormalConcept] -> String
showConcepts [] = "\n"
showConcepts (fc@(FormalConcept (s,n,a)):ls) = (GV.showGV fc) ++ " Extent: " ++ (show n) ++ "\nIntent: " ++ (show a) ++ "\n\n" ++ (showConcepts ls)

fcDiffButSameGM :: FormalConcept -> FormalConcept -> Bool
fcDiffButSameGM (FormalConcept (n1,g1,m1)) (FormalConcept (n2,g2,m2)) = (S.fromList g1) == (S.fromList g2) && (S.fromList m1) == (S.fromList m2) && n1 /= n2

mergeLabels :: [Label] -> Label
mergeLabels ls = foldl (\(g,m) (sg,sm) -> (sg++g, sm++m)) ([],[]) ls

-- | Combines identical concepts with the same extent & intent, by joining their names & tossing one
combineIdenticalConcepts :: [FormalConcept] -> [FormalConcept]
combineIdenticalConcepts ls = combineIC ls ls
                              where
                                combineIC :: [FormalConcept] -> [FormalConcept] -> [FormalConcept]
                                combineIC [] _ = []
                                combineIC (fc@(FormalConcept (n,g,m)):xs) ys = let dups = filter (fcDiffButSameGM fc) ys in
                                                                               let dupNames = if length dups > 0 then map conceptName dups else [] in
                                                                               let fc2 = (FormalConcept (mergeLabels $ n : dupNames, g, m)) in
                                                                               let filt = filter (not . (fcDiffButSameGM fc)) xs in
                                                                               fc2 : combineIC filt ys

-- Produce intersections that do not already exist
intersectExtents :: [[Object]] -> [Object] -> [[Object]]
intersectExtents xs y = filter (\z -> not (elem z xs) && z /= []) $ map (intersect y) xs

getConceptsFromContext :: ConceptsBy -> FormalContext -> [FormalConcept]
-- Focuses on attribute concepts
getConceptsFromContext ConceptsByIntents fc = combineIdenticalConcepts $ mkFormalConceptFromIntents fc
-- Focuses on object concepts
getConceptsFromContext ConceptsByExtents fc = combineIdenticalConcepts $ mkFormalConceptFromExtents fc
getConceptsFromContext AllConcepts fc@(_,fcm,_) = let attrConcepts = combineIdenticalConcepts $ mkFormalConceptFromIntents fc in -- compute attribute concepts
                                        let acExtents = map conceptExtent attrConcepts in -- compute extents of these concepts
                                        let acIntersections = concatMap (intersectExtents acExtents) acExtents in -- intersect these extents
                                        let extraConcepts = map (\ogi -> FormalConcept (([],[]), ogi, a' ogi fc)) acIntersections in -- generate the extra concepts
                                        uniqueInSameOrder $ combineIdenticalConcepts $ attrConcepts ++ extraConcepts ++ (mkFormalConceptFromExtents fc) -- put it all together


-- Convert program in AST to list Terms
astToTerms :: (Data a, Show b) => (String -> Maybe b) -> (Object,a) -> (Object,[Term])
astToTerms f (name,p) = (name, uniqueInSameOrder $ map show $ catMaybes $ map f $ mkTermsFromData p)

-- Traverse immediate subterms recursively to discover all terms used
mkTermsFromData :: Data a => a -> [Term]
mkTermsFromData val =
                let tn = show $ typeOf val in -- 1. get type name
                -- 2. get constructor name
                let cn = if isAlgType (dataTypeOf val) then show $ toConstr val else tn in
                -- 3. same for subterms
                let immediateAnds = gmapQ (\d -> show $ typeOf d) val in
                -- run this for sub-terms as well
                let subs = concat $ gmapQ (\d -> mkTermsFromData d) val in
                tn : cn : subs ++ immediateAnds

-- 3) Function that produces a formal context from program names & terms
mkFormalContext :: [(Object,[Attribute])] -> FormalContext
mkFormalContext ls = let g = map (\(obj,_)  -> obj) ls in
                     let m = uniqueInSameOrder $ concatMap (\(_,atrs) -> atrs) ls in
                     let i = uniqueInSameOrder $ concatMap (\(obj,atrs) -> map (\atr -> (obj,atr)) atrs) ls in
                     (g, m, i)

-- ' : A -> B, returns set of attributes which apply to all objects in A
a' :: [Object] -> FormalContext -> [Attribute]
a' [] _ = []
a' (x:ls) fc@(_,_,i) = let filt = filter (\(o,_) -> o == x) i in
                       let cm = map snd filt in
                       let rset = S.fromList (a' ls fc) in
                       let atrs = if null rset then cm else S.toList $ (S.fromList cm) `S.intersection` rset in
                       uniqueInSameOrder $ atrs

-- ' : B -> A, returns set of objects which have all attributes in B
b' :: [Attribute] -> FormalContext -> [Object]
b' [] _ = []
b' (x:ls) fc@(_,_,i) = let filt = filter (\(_,a) -> a == x) i in
                       let cm = map fst filt in
                       let rset = S.fromList (b' ls fc) in
                       let atrs = if null rset then cm else S.toList $ (S.fromList cm) `S.intersection` rset in
                       uniqueInSameOrder $ atrs

-- find formal concepts through object analysis
-- Set A of objs can determine a concept
-- (A'',A')
-- A'' is the extent closure
mkFormalConceptFromExtents :: FormalContext -> [FormalConcept]
mkFormalConceptFromExtents fc@(g,m,i) = map (\obj -> FormalConcept (([obj],[]), b' (a' [obj] fc) fc, a' [obj] fc)) g

-- produce a sub-set of concepts from a given set of objects
-- which are implied to be in the formal context, otherwise this does not work...
mkFormalConceptsFromSubExtents :: FormalContext -> [Object] -> [FormalConcept]
mkFormalConceptsFromSubExtents fc g = map (\obj -> FormalConcept (([obj],[]), b' (a' [obj] fc) fc, a' [obj] fc)) g

-- find formal concepts through attribute analysis
-- or set B of attributes determines a concept
-- (B',B'')
-- B'' is the intent closure
mkFormalConceptFromIntents :: FormalContext -> [FormalConcept]
mkFormalConceptFromIntents fc@(g,m,i) = map (\atr -> FormalConcept (([],[atr]), b' [atr] fc, a' (b' [atr] fc) fc)) m

-- partially ordered by extents
-- (A1,B1) <= (A2,B2) iff A1 is a subset A2
compareFormalConceptsByExtents :: FormalConcept -> FormalConcept -> Ordering
compareFormalConceptsByExtents (FormalConcept (_,a,_)) (FormalConcept (_,b,_)) = case (S.fromList a) `S.isSubsetOf` (S.fromList b) of
                                                                              True -> if a == b then EQ else LT
                                                                              False-> GT

-- partially ordered by intents (these 2 should be equivalent ways of doing this)
-- (A1,B1) <= (A2,B2) iff B2 is a subset B1
compareFormalConceptsByIntents :: FormalConcept -> FormalConcept -> Ordering
compareFormalConceptsByIntents (FormalConcept (_,_,a)) (FormalConcept (_,_,b)) = case (S.fromList b) `S.isSubsetOf` (S.fromList a) of
                                                                              True -> if a == b then EQ else LT
                                                                              False-> GT

-- a mark can be some mark or no mark
data Mark = Some String | None

instance Show Mark where
  show (Some s) = s
  show None = "."

chooseMatElm :: FormalContext -> (Int,Int) -> Mark
chooseMatElm (g,m,i) (x,y) = case (x,y) of -- x == 1
                              (1,1) -> Some ""
                              -- show attributes
                              (1,_)  -> Some $ "M" ++ (show (y-1)) --
                              -- show objects
                              (_,_) -> Some $ "P" ++ (show (x-1)) -- ++ idName (x-1) -- (old) Some $ show $ g !! (x-2)

showMatLegend :: [(String,String)] -> String
showMatLegend [] = ""
showMatLegend ((k,v):ls) = k ++ " -> " ++ v ++ "\n" ++ (showMatLegend ls)

-- builds a 2D matrix
mkFormalConceptMatrix :: FormalContext -> ([(String,String)],[(String,String)],Matrix Mark)
mkFormalConceptMatrix (g,m,i) = (map (\x -> ("P" ++ show x, g !! (x-1))) [1..(length g)], map (\y -> ("M" ++ show y, m !! (y-1))) [1..(length m)], matrix ((length g) + 1) ((length m) + 1) (\(x,y) -> if x == 1 || y == 1 then chooseMatElm (g,m,i) (x,y) else let gx = g !! (x-2) in
                                                                        let my = m !! (y-2) in
                                                                        case find (\q -> q == (gx,my)) i of
                                                                          Just _  -> Some "X"
                                                                          Nothing -> None))

-- builds a 2D matrix
mkFormalConceptMatrixSmall :: FormalContext -> Matrix Mark
mkFormalConceptMatrixSmall (g,m,i) = matrix ((length g)) ((length m)) (\(x,y) -> let gx = g !! (x-1) in
                                                                                 let my = m !! (y-1) in
                                                                                    case find (\q -> q == (gx,my)) i of
                                                                                        Just _  -> Some "X"
                                                                                        Nothing -> None)
-- But maybe handy to have?
join :: String -> [String] -> String
join _ [] = ""
join c (x:ls) = if length ls > 0 then x ++ c ++ (join c ls) else x

-- Builds a ConceptLattice from a list of formal concepts
mkConceptLattice :: [FormalConcept] -> ConceptLattice
mkConceptLattice xs = (xs, _mkConceptLattice xs xs)

-- internally constructs the relationships between formal concepts as edges
_mkConceptLattice :: [FormalConcept] -> [FormalConcept] -> [(FormalConcept,FormalConcept)]
_mkConceptLattice [] _ = []
_mkConceptLattice (x:ls) bs = let subConcepts = filter (\y -> y < x) bs in -- find all subconcepts
                             -- get the most general subconcepts that are direct neighbors of concept 'x'
                             let neighbors = filter (\q -> all (\z -> q > z || q == z) subConcepts) subConcepts in
                             -- creates edges from x -> neighbors
                             let edges = map (\z -> (x,z)) neighbors in
                             edges ++ (_mkConceptLattice ls bs)

--
-- TODO attribute implication work
--
getTT :: FormalContext -> [[Attribute]]
getTT (g,m,i) = reverse $ map (getAttrList i) g
                             where
                               getAttrList :: [(Object,Attribute)] -> Object -> [Attribute]
                               getAttrList i o = map snd $ filter (\(a,_) -> a == o) i

deriveImplications :: [Attribute] -> [[Attribute]] -> [Implication]
deriveImplications [] _ = []
deriveImplications (x:xs) ys = let x1 = map (\\ [x]) (filter (\yy -> elem x yy) ys) in -- retrieve all rows that have 'x' present (and remove 'x' as a possible premise)
                               let inter = foldl intersect (x1 !! 0) (tail x1) in -- compute the intersection of all rows that have 'x' present
                               (inter,[x]) : (deriveImplications xs ys) -- use the intersection of all rows as an implication for attribute 'x'

-- | Remove conflicting implications
-- TODO This rule is the problem, can probably re worked to better capture what I want it to do
removeConflictingImplications :: [Implication] -> [Implication]
removeConflictingImplications ls = filter (\(p,c) -> length c == 1) (_mi ls ls) -- only allow implications to pass that are valid, i.e. unambiguous
                       where
                        _mi :: [Implication] -> [Implication] -> [Implication]
                        _mi [] _ = []
                        _mi ((p,_):xs) ys = let filt = filter (\(p',c') -> p == p') ys in
                                            case filt of
                                              []    -> _mi xs ys -- no matches, ignore it
                                              --[one] -> (one) : (_mi xs ys) -- 1 match is itself, move along
                                              filt' -> (p,concatMap snd filt') : (_mi xs (ys \\ filt'))

filterCommonPremise :: [Attribute] -> [Implication] -> [Implication]
filterCommonPremise as ls = let inter = (foldl (\p' s -> intersect p' s) (fst $ ls !! 0) (map fst ls)) ++ as in
                             DT.trace ("Common intersection is " ++ (show inter)) $ filter (not . null . fst) $ map (\(p,c) -> (p \\ inter, c)) ls

stringImplications :: [Implication] -> IO ()
stringImplications [] = do putStrLn "\n"
stringImplications ((p,c):ls) = do
  putStrLn $ (join " ^ " $ sort p) ++ "\n\t-> " ++ (join " v " $ sort c)
  _ <- (stringImplications ls)
  return ()
