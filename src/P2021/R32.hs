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

-- Pair of sets, (A subset G, B subset M)
-- A is the extent: set of all objs that have all the attributes in B
  -- For all 'a' in G. 'a' in A iff (For all 'b' in B. b(a))
-- B is the intent: set of all attributes that match to all objects in A
  -- For all 'b' in M. 'b' in B iff (For all 'a' in A. b(a))
-- Also has a name, for easy id
data FormalConcept = FormalConcept (String,[Object],[Attribute])
  deriving (Eq,Show)

conceptName :: FormalConcept -> String
conceptName (FormalConcept (n,_,_)) = n

conceptIntent :: FormalConcept -> [String]
conceptIntent (FormalConcept (_,_,m)) = m

conceptExtent :: FormalConcept -> [String]
conceptExtent (FormalConcept (_,g,_)) = g

instance Ord FormalConcept where
  (<=) a b = let r = compareFormalConceptsByExtents a b in r /= GT
  (<) a b = let r = compareFormalConceptsByExtents a b in r == LT
  (>) a b = let r = compareFormalConceptsByExtents a b in r == GT
  compare a b = compareFormalConceptsByExtents a b

instance GV.ShowGV FormalConcept where
  showGV (FormalConcept (n,_,_)) = n

-- formal context (G,M,I)
-- G = Set of all objects (programs)
-- M = Set of all attributes (terms)
-- I = Subset of (G X M), Relation that links elements of G to elements of M
type FormalContext = ([Object],[Attribute],[(Object,Attribute)])

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

type KnownPrograms = [ConcreteProgram]
type GoalPrograms  = [ConcreteProgram]
type CoursePrograms= [ConcreteProgram]

-- Configuration for performing Formal Concept Analysis
data FCA a = FCA OrderBy ConceptsBy (ParsingFunction a) KnownPrograms GoalPrograms CoursePrograms

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

-- returns supremum of the concept lattice (Top most node)
getJoin :: ConceptLattice -> FormalConcept
getJoin (nodes,_) = head $ reverse $ sort nodes

-- returns the infimum of the concept lattice (Bottom most node)
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

-- ([ConcreteProgram] -> [(String, a)]) -> [ConcreteProgram] -> [ConcreteProgram]
r32 :: (Data a, Show a) => FCA a -> IO ()
r32 (FCA ob cb programParser kps gps cps) = do
  -- parse the programs that are understood
  let parsedKPS = programParser kps
  -- parse the goal programs that are desired to be understood
  let parsedGPS = programParser gps
  -- parse course programs that are available as extents
  let parsedCPS = programParser cps

  -- extract terms for the known list of programs, preserving names
  let knownIntents = map astToTerms parsedKPS
  -- extract terms for the goal list of programs, preserving names
  let goalIntents = map astToTerms parsedGPS
  -- extract terms for the course list of programs, preserving names
  let courseIntents = map astToTerms parsedCPS

  -- produce total context
  let totalContext = mkFormalContext (uniqueInSameOrder (knownIntents ++ goalIntents ++ courseIntents))

  -- TODO tmp experiment
  --let totalContext = filterGeneralAttributes totalContext_
  -- produce Known + Goal context
  --let kgContext = mkFormalContext (uniqueInSameOrder (knownIntents ++ goalIntents))

  -- produce total concepts from total context
  let totalConcepts = getConceptsFromContext cb totalContext

  -- produce Known + Goal concepts from K+G context
  -- find the smallest & largest concept in K+G list
  --let smallest = minimum kgConcepts
  --let largest  = maximum kgConcepts
  -- TODO thinking this filtering can be done by getting the smallest that has the known & largest that has the goal concepts from the universe...that would work right?
  -- hmmm, need to read some more, but I think I can lay this out pretty clearly
  -- remove all concepts beyond this range of concepts
  --let filteredConcepts = if length kgConcepts > 1 then filter (\x -> x >= smallest && x <= largest) totalConcepts else filter (\x -> x <= largest) totalConcepts

  putStrLn $ "Num Course Progs: " ++ (show $ length cps)
  putStrLn $ "Num Course Objects: " ++ (show $ length ((\(x,_,_) -> x) totalContext))
  putStrLn $ "Num Course attributes: " ++ (show $ length ((\(_,x,_) -> x) totalContext))
  putStrLn $ "Num Domain Concepts: " ++ (show $ length totalConcepts)
  putStrLn $ showConcepts (sortBy (getConceptOrdering ob) totalConcepts)

  -- TODO TODO TESTING!
  -- all intents
  let m' = FormalConcept ("",[],(\(_,m,_) -> m) totalContext)
  -- all extents
  let g' = FormalConcept ("",(\(a,_,_) -> a) totalContext,[])
  let l1 = if doesConceptExtentAlreadyExist g' totalConcepts then totalConcepts else totalConcepts ++ [g']
  let l2 = if doesConceptIntentAlreadyExist m' l1 then l1 else l1 ++ [m']
  --  add & filter out to avoid dups
  let sorted = reverse $ sort $ l2
  -- updates concepts names to C1,C2,...CN
  --let updatedConcepts = updateConceptNames 1 sorted

  -- TODO duplicate extents are normal, as intents vary, and already filter them in generating our TOTAL CONCEPT list
  -- removes duplicate extents
  --putStrLn $ "Pre-Dub Extent count is " ++ (show $ length sorted)
  --let tc2' = filterOutDuplicateExtents sorted sorted
  --putStrLn $ "TC2' count is " ++ (show $ length tc2')

  let conceptLattice = mkConceptLattice sorted

  --let prettyLattice = mapL conceptName conceptLattice
  graphConceptLattice "R32_Test_1" conceptLattice

  -- TODO full detailed formal context as a matrix
  --let (pm,mm,matt) = mkFormalConceptMatrix totalContext
  --putStrLn $ showMatLegend pm
  --putStrLn $ showMatLegend mm
  --putStrLn $ prettyMatrix matt
  putStrLn $ prettyMatrix (mkFormalConceptMatrixSmall totalContext)







-- TODO PROBABLY REMOVE
-- Compute Concept Intents for the labeling on the concept lattice
getNewIntents :: [FormalConcept] -> [FormalConcept] -> [String] -> ([FormalConcept],[String])
getNewIntents [] fcs i = (fcs,i)
getNewIntents ((FormalConcept (n,g,m)):xs) ys i = let conceptIntent = m \\ i
                                                      i2 = conceptIntent ++ i
                                                      fc2 = FormalConcept (if length conceptIntent > 0 then n ++ " : " ++ (join "," conceptIntent) else n,g,m)
                                                  in getNewIntents xs (fc2:ys) i2

_computeConceptIntentLabels :: [FormalConcept] -> [String] -> ConceptLattice -> ConceptLattice
_computeConceptIntentLabels [] _ cl = cl
_computeConceptIntentLabels (x:ls) intents cl1 = let nxts = getLowerNeighbors cl1 x in -- get nxt nodes
                                                 --let (x2,intents') = getNewIntents [x] [] intents in
                                                 let (nxts2,intents2) = getNewIntents nxts [] intents in
                                                 let cl2 = foldl (\cli fc -> updateInLattice cli fc) cl1 nxts2 in
                                                 _computeConceptIntentLabels (ls ++ nxts) intents2 cl2

computeConceptIntentLabels :: ConceptLattice -> ConceptLattice
computeConceptIntentLabels cl = let join = getJoin cl in _computeConceptIntentLabels [join] (conceptIntent join) cl









-- TODO PROBABLY REMOVE
-- Compute Concept Extents for the labeling on the concept lattice
getNewExtents :: [FormalConcept] -> [FormalConcept] -> [String] -> ([FormalConcept],[String])
getNewExtents [] fcs g = (fcs,g)
getNewExtents ((FormalConcept (n,g,m)):xs) ys i = let conceptExtent = g \\ i
                                                      i2 = conceptExtent ++ i
                                                      fc2 = FormalConcept (if length conceptExtent > 0 then n ++ " : " ++ (join "," conceptExtent) else n,g,m)
                                                  in getNewExtents xs (fc2:ys) i2

_computeConceptExtentLabels :: [FormalConcept] -> [String] -> ConceptLattice -> ConceptLattice
_computeConceptExtentLabels [] _ cl = cl
_computeConceptExtentLabels (x:ls) extents cl1 = let nxts = getUpperNeighbors cl1 x in -- get nxt nodes
                                                 --let (x2,extents') = getNewExtents [x] [] extents in
                                                 let (nxts2,extents2) = getNewExtents nxts [] extents in
                                                 let cl2 = foldl (\cli fc -> updateInLattice cli fc) cl1 nxts2 in
                                                 _computeConceptExtentLabels (ls ++ nxts) extents2 cl2

computeConceptExtentLabels :: ConceptLattice -> ConceptLattice
computeConceptExtentLabels cl = let meet = getMeet cl in _computeConceptExtentLabels [meet] (conceptExtent meet) cl










updateConceptNames :: Int -> [FormalConcept] -> [FormalConcept]
updateConceptNames _ [] = []
updateConceptNames x ((FormalConcept (_,a,b)):ls) = DT.trace ("C" ++ (show x) ++ " -> " ++ (show a)) $ (FormalConcept ("C" ++ (show x),a,b)) : updateConceptNames (x+1) ls

-- determine whether a concept extent already exists
doesConceptExtentAlreadyExist :: FormalConcept -> [FormalConcept] -> Bool
doesConceptExtentAlreadyExist (FormalConcept (_,extent,_)) ls = any (\(FormalConcept (_,g,_)) -> S.fromList g == S.fromList extent) ls

-- determine whether a concept intent already exists
doesConceptIntentAlreadyExist :: FormalConcept -> [FormalConcept] -> Bool
doesConceptIntentAlreadyExist (FormalConcept (_,_,intent)) ls = any (\(FormalConcept (_,_,m)) -> S.fromList m == S.fromList intent) ls


{-
filterOutDuplicateExtents :: [FormalConcept] -> [FormalConcept] -> [FormalConcept]
filterOutDuplicateExtents [] _ = []
filterOutDuplicateExtents (fc@(FormalConcept (n1,a,_)):ls) bs = let n = filter (\(FormalConcept (n2,b,_)) -> S.fromList a == S.fromList b) bs in
                                                             if length n > 1 then filterOutDuplicateExtents ls bs else fc : filterOutDuplicateExtents ls bs
-}

getConceptOrdering :: OrderBy -> (FormalConcept -> FormalConcept -> Ordering)
getConceptOrdering OrderByIntents = compareFormalConceptsByIntents
getConceptOrdering OrderByExtents = compareFormalConceptsByExtents

showConcepts :: [FormalConcept] -> String
showConcepts [] = "\n"
showConcepts ((FormalConcept (s,n,a)):ls) = "(" ++ s ++ ") Extent: " ++ (show n) ++ "\nIntent: " ++ (show a) ++ "\n\n" ++ (showConcepts ls)

fcDiffButSameGM :: FormalConcept -> FormalConcept -> Bool
fcDiffButSameGM (FormalConcept (n1,g1,m1)) (FormalConcept (n2,g2,m2)) = (S.fromList g1) == (S.fromList g2) && (S.fromList m1) == (S.fromList m2) && n1 /= n2

-- | Combines identical concepts with the same extent & intent, by joining their names & tossing one
combineIdenticalConcepts :: [FormalConcept] -> [FormalConcept]
combineIdenticalConcepts ls = combineIC ls ls
                              where
                                combineIC :: [FormalConcept] -> [FormalConcept] -> [FormalConcept]
                                combineIC [] _ = []
                                combineIC (fc@(FormalConcept (n,g,m)):xs) ys = let dups = filter (fcDiffButSameGM fc) ys in
                                                                               let dupNames = if length dups > 0 then (join "," (map conceptName dups)) else "" in
                                                                               let fc2 = (FormalConcept (n++dupNames,g,m)) in
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
getConceptsFromContext AllConcepts fc = let objectConcepts = combineIdenticalConcepts $ mkFormalConceptFromExtents fc in
                                        let objectConcepts' = map (\(FormalConcept (n,g,m)) -> (FormalConcept (n++" / ",g,m))) objectConcepts in
                                        let ocExtents = map conceptExtent objectConcepts' in
                                        let ocIntersections = concatMap (intersectExtents ocExtents) ocExtents in
                                        let extraConcepts = map (\ogi -> FormalConcept ("",b' (a' ogi fc) fc, a' ogi fc)) ocIntersections in
                                        uniqueInSameOrder $ combineIdenticalConcepts $ objectConcepts' ++ extraConcepts ++ (mkFormalConceptFromIntents fc)


-- Convert program in AST to list Terms
astToTerms :: (Data a) => (Object,a) -> (Object,[Term])
astToTerms (name,p) = (name, uniqueInSameOrder $ mkTermsFromData p)

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
mkFormalConceptFromExtents fc@(g,m,i) = map (\obj -> FormalConcept (obj,b' (a' [obj] fc) fc, a' [obj] fc)) g

-- find formal concepts through attribute analysis
-- or set B of attributes determines a concept
-- (B',B'')
-- B'' is the intent closure
mkFormalConceptFromIntents :: FormalContext -> [FormalConcept]
mkFormalConceptFromIntents fc@(g,m,i) = map (\atr -> FormalConcept (atr,b' [atr] fc, a' (b' [atr] fc) fc)) m

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
mkFormalConceptMatrixSmall (g,m,i) = matrix ((length g) - 1) ((length m) - 1) (\(x,y) -> let gx = g !! (x) in
                                                                             let my = m !! (y) in
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



-- ehh, may not be doing an outer/inner fringe if we're not using KST
-- 6) Add a function that identifies the outer & inner fringe based on [Known FormalConcept] and the CG
-- ConceptGraph ... -> [Known FormalConcept]
  -- from here, report the outer & inner fringe, and leave it to the user to proceed separately
  -- these would be the items directly preceding the highest level of the known area (or just the boundary in general)
  -- and same for the ones just before
  -- this can be identified if we have a sub-lattice that works.

-- 7) Record as (Concept Graph of FCs,[Known FCs])
  -- for practical observation need a way to observe the possible set of paths that will be generated (all possible progressions)
  -- just recursively build this up, should be okay?
