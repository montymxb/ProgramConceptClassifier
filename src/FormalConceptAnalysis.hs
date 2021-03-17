{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
--
-- FormalConceptAnalysis implementation
--
module FormalConceptAnalysis where

import Data.Data
import General
import qualified Data.Set as S
import Data.List
import Data.Matrix
import Data.Maybe
import qualified Data.PartialOrd as PO
import qualified GraphViz as GV
import qualified Debug.Trace as DT

-- terms are names in this context, both terminal & nonterminal
type Term = String

-- Object with attributes
type Object = String

-- attribute of an object
type Attribute = String

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

conceptLabel :: FormalConcept -> Label
conceptLabel (FormalConcept (n,_,_)) = n

conceptIntent :: FormalConcept -> [Attribute]
conceptIntent (FormalConcept (_,_,m)) = m

conceptExtent :: FormalConcept -> [Object]
conceptExtent (FormalConcept (_,g,_)) = g

instance PO.PartialOrd FormalConcept where
  (<=) a b = case compareFormalConceptsByExtents a b of
              Just r  -> r /= GT
              Nothing -> False
  (==) a b = case compareFormalConceptsByExtents a b of
              Just r  -> r == EQ
              _       -> False
  (<) a b = case compareFormalConceptsByExtents a b of
              Just r  -> r == LT
              Nothing -> False

  (>=) a b = case compareFormalConceptsByExtents a b of
              Just r  -> r /= LT
              Nothing -> False
  (>) a b = case compareFormalConceptsByExtents a b of
              Just r  -> r == GT
              Nothing -> False

underscoreReplace :: String -> String
underscoreReplace = map (\x -> if x == '_' then ' ' else x)

instance GV.GraphVizable FormalConcept where
  node (FormalConcept (([],[]),_,_))  = [("label","")]
  node (FormalConcept (([],_),_,_))   = [("shape","point")] -- point marks
  node (FormalConcept ((g,_),_,_))    = [("label",join ", " (map underscoreReplace g)),("fontcolor","purple"),("fontname","Helvetica")]
  edge (FormalConcept ((_,a),_,_)) (FormalConcept ((_,b),_,_)) = let d = (b \\ a) in
                                                                 if length d > 0 then
                                                                   [("label",join ", " (map underscoreReplace d)),("fontname","Helvetica"),("fontcolor","darkorange")]
                                                                 else
                                                                   []

-- formal context (G,M,I)
-- G = Set of all objects (programs)
-- M = Set of all attributes (terms)
-- I = Subset of (G X M), Relation that links elements of G to elements of M
type FormalContext = ([Object],[Attribute],[(Object,Attribute)])

contextExtent :: FormalContext -> [Object]
contextExtent (g,_,_) = g

contextIntent :: FormalContext -> [Attribute]
contextIntent (_,m,_) = m

type ConceptMapping b = (String -> Maybe b)

type KnownPrograms a  = [(String,a)]
type GoalPrograms a   = [(String,a)]
type CoursePrograms a = [(String,a)]

-- Configuration for performing Formal Concept Analysis
data FCA a b = FCA (ConceptMapping b) (KnownPrograms a) (GoalPrograms a) (CoursePrograms a) [Object] [b]

type ConceptLattice = ([FormalConcept],[(FormalConcept,FormalConcept)])

-- | Return the lower neighbors of this node in the concept lattice, if any
getLowerNeighbors :: ConceptLattice -> FormalConcept -> [FormalConcept]
getLowerNeighbors (_,edges) fc = map snd $ filter (\(i,_) -> i PO.== fc) edges

-- | Return the upper neighbors of this node in the concept lattice, if any
getUpperNeighbors :: ConceptLattice -> FormalConcept -> [FormalConcept]
getUpperNeighbors (_,edges) fc = map fst $ filter (\(_,o) -> o PO.== fc) edges

-- | Get all upper neighbors from a classification
getAllUpperNeighbors :: [FormalConcept] -> FormalConcept -> [FormalConcept]
getAllUpperNeighbors xs fc = let un = PO.maxima $ filter (\y -> y PO.> fc) xs in
                             case un of
                               [] -> []
                               _  -> concatMap (getAllUpperNeighbors xs) un

-- | Get all lower neighbors from a classification
getLowerNeighbors' :: [FormalConcept] -> FormalConcept -> [FormalConcept]
getLowerNeighbors' xs fc = PO.maxima $ filter (\y -> y PO.< fc) xs --in
                             --case un of
                            --   [] -> []
                            --   _  -> concatMap (getAllLowerNeighbors xs) un

-- returns supremum of the concept lattice (Top most node, most general, concept of all objects)
getJoin :: ConceptLattice -> FormalConcept
getJoin (nodes,_) = head $ PO.maxima nodes

-- returns the infimum of the concept lattice (Bottom most node, most specific, concept of all attributes)
getMeet :: ConceptLattice -> FormalConcept
getMeet (nodes,_) = head $ PO.minima nodes

-- updates a single concept in both the nodes & edges portion and returns the updated lattice
-- focuses on updating name only
{-
updateInLattice :: ConceptLattice -> FormalConcept -> ConceptLattice
updateInLattice (nodes,edges) fc@(FormalConcept (_,a,b)) = let n = map maybeReplace nodes in
                                                           let e = map (\(f1,f2) -> (maybeReplace f1, maybeReplace f2)) edges in
                                                           (n,e) where
                                                             maybeReplace :: FormalConcept -> FormalConcept
                                                             maybeReplace fc2@(FormalConcept (_,a2,b2)) = if a == a2 && b == b2 then fc else fc2
-}

data Wrapper = Wrap String

instance Show Wrapper where
  show (Wrap s) = s

asIsConceptMapping :: String -> Maybe Wrapper
asIsConceptMapping s = Just (Wrap s)

-- knowledge state is modeled with
-- [KnownClassifications] [KnownPrograms] [KnownAttributes]
data KnowledgeState = KnowledgeState [FormalConcept] [Object] [Attribute]
  deriving Show

-- | Gets the next programs, by concepts that don't have empty object labels
getNextPrograms :: [FormalConcept] -> ConceptLattice -> [FormalConcept]
getNextPrograms [] _  = []
getNextPrograms ks cl = let lf = filter (\x -> not $ null (fst $ conceptLabel x)) ks in
                        let le = filter (\x -> null (fst $ conceptLabel x)) ks in
                        lf ++ (getNextPrograms (concatMap (getLowerNeighbors cl) le) cl)


-- This is for TESTING, the actual setup will not be an IO monad (essentially a prototyping sandbox)
-- run the analysis, taking an FCA analysis instance
-- Takes known programs, goal programs, and course programs (programs available in the course)
-- returns a Dot specification & a list of programs in the outer 'fringe'
fca :: (Data a, Show a, Subsumable b, Show b) => FCA a b -> IO (String,[(String,[String])])
fca (FCA conceptMapping kps gps cps extraKnownProgs extraKnownIntents) = do
  -- extract terms for the these program lists, preserving names
  -- ### STEP 1: extract AST, (program,[term])
  let knownTaggedPrograms   = S.toList $ S.fromList $ map (astToTerms conceptMapping) kps
  let goalTaggedPrograms    = S.toList $ S.fromList $ map (astToTerms conceptMapping) gps
  let courseTaggedPrograms  = S.toList $ S.fromList $ map (astToTerms conceptMapping) cps

  -- ### STEP 2: produce formal context
  -- produce total context
  -- from known & goal programs, we want to create an 'intent of interest'
  -- we will use this to reduce the attributes we have in our graph to only those we care about (between goal & known inclusively)
  -- filter by lower intent, removing any programs that are less any other program we 'know'
  let fcp1 = case knownTaggedPrograms of
              [] -> courseTaggedPrograms
              _  -> filter (\(_,b) -> all (\z -> (not $ S.fromList b `S.isSubsetOf` S.fromList z)) (map snd knownTaggedPrograms)) courseTaggedPrograms
  -- filter by upper intent, including any program that is a subset of any goal program
  let filteredCourseProgs = case goalTaggedPrograms of
              [] -> fcp1
              _  -> filter (\(_,b) -> any (\z -> S.fromList b `S.isSubsetOf` S.fromList z) (map snd goalTaggedPrograms)) fcp1
  -- generate decomposed formal context
  let (g',m',i') = mkFormalContext (uniqueInSameOrder' $ knownTaggedPrograms ++ goalTaggedPrograms ++ filteredCourseProgs)
  -- only reduce the context against the upper intent (include only those programs & concepts that are within that scope)
  -- removes extraneous detail that is unrelated to this query
  let totalContext = case (uniqueInSameOrder $ concatMap snd goalTaggedPrograms) of
                        []  -> (g',m',i')
                        ul  -> (g', filter (`elem` ul) m', filter (\(_,b) -> b `elem` ul) i')

  -- ### STEP 3: get formal concepts
  -- produce total concepts from total context
  let totalConcepts' = getConceptsFromContext totalContext
  -- get known formal concepts, factoring in those that have been implicitly indicated by the programs & attributes added so far while learning
  let kfc = catMaybes $ map (findObjectConcept totalConcepts') (map fst knownTaggedPrograms)
  -- find known concepts plus any concepts that we could additionally mark as known (ones that are subsets of what we indicated we have learned so far, beyond the known program)
  -- TODO, this may be unnecessary now (the addition of the other concepts from the filter)
  -- The only reason this is here was to make 'Concepts' and 'Programs' explicit by hand...but not sure I want to do that anymore (it's not in our specification, should be dropped)
  let knownFormalConcepts = (filter (\(FormalConcept ((g,m),_,_)) -> S.fromList g `S.isSubsetOf` S.fromList extraKnownProgs && S.fromList m `S.isSubsetOf` S.fromList (map show extraKnownIntents) && (not $ S.null $ S.fromList g) && (not $ S.null $ S.fromList m)) totalConcepts')
  --let knownFormalConcepts = knownFormalConcepts' ++ (filter (\x -> all (x PO.>) knownFormalConcepts') totalConcepts')
  -- the above line should have done this, but it seems to have failed in this regard
  let goalFormalConcepts = catMaybes $ map (findObjectConcept totalConcepts') (map fst goalTaggedPrograms)
  -- Apply Bounding concepts to get sub-lattice to work with
  let boundingConcepts = catMaybes $ map (findObjectConcept totalConcepts') ((map fst knownTaggedPrograms) ++ (map fst goalTaggedPrograms))

  --putStrLn $ show $ map conceptLabel knownFormalConcepts
  --putStrLn $ "Pre-filter Classification Count: " ++ (show $ length totalConcepts')

  let upKnownNeigh = concatMap (getAllUpperNeighbors totalConcepts') knownFormalConcepts
  let upGoalNeigh = concatMap (getAllUpperNeighbors totalConcepts') goalFormalConcepts

  let totalConcepts = case (length knownFormalConcepts > 0, length goalFormalConcepts > 0) of
                        -- no bounds
                        (False,False) -> totalConcepts'
                        -- TODO lower bounds seem good now, but upper bounds seem like the old way worked better then what I tried to change it to
                        -- i.e. the PO approach seems to work there
                        -- upper & lower bounds, we want programs that are more specific than the most general program
                        -- and are more general than the most specific program (bounded set)
                        (True,True)   -> filter (\x -> (not $ elem x upKnownNeigh || elem x knownFormalConcepts) || (elem x upGoalNeigh || elem x goalFormalConcepts)) totalConcepts'
                        --(True,True)   -> filter (\x -> (not $ elem x upKnownNeigh || elem x knownFormalConcepts) && any (x PO.>=) (PO.minima boundingConcepts)) totalConcepts'
                        -- lower bound, we only want programs more specific than this set (smaller extent)
                        (True,False)  -> filter (\x -> not $ elem x upKnownNeigh || elem x knownFormalConcepts) totalConcepts'
                        --(True,False)  -> filter (\x -> any (x PO.<) (PO.maxima boundingConcepts)) totalConcepts'
                        -- upper bound, we only want programs more general than this set (larger extent)
                        --(False,True)  -> filter (\x -> elem x upGoalNeigh || elem x goalFormalConcepts) totalConcepts'
                        (False,True)  -> filter (\x -> any (x PO.>=) (PO.minima boundingConcepts)) totalConcepts'

  --let dnKnownNeigh = concatMap (getLowerNeighbors' totalConcepts') knownFormalConcepts
  --let dnNextProgs = concatMap (fst . conceptLabel) (getNextPrograms knownFormalConcepts totalConcepts')
  --let totalConcepts = filter (\x -> elem x dnKnownNeigh) totalConcepts''

  {-
  putStrLn $ "Num Course Progs: " ++ (show $ length cps)
  putStrLn $ "Num Course Objects: " ++ (show $ length ((\(x,_,_) -> x) totalContext))
  putStrLn $ "Num Course attributes: " ++ (show $ length ((\(_,x,_) -> x) totalContext))
  putStrLn $ "Num Formal Concepts (Program,Attribute set pairs): " ++ (show $ length totalConcepts)
  putStrLn $ showConcepts totalConcepts
  -}

  -- report Known Concepts
  --let knownConcepts = S.toList $ S.fromList (concatMap conceptIntent knownFormalConcepts)
  --putStrLn $ "Known Concepts: " ++ show knownConcepts

  -- report Goal concepts (minus Known), these represent all concepts in the reduced set
  --let goalConcepts = (S.toList $ S.fromList (concatMap conceptIntent goalFormalConcepts)) \\ knownConcepts
  --putStrLn $ "Goal Concepts: " ++ show goalConcepts


  -- ### STEP 4:  produce concept lattice
  -- create Formal Concept of all intents
  let fcm = FormalConcept (([],[]), [], contextIntent totalContext)
  -- create Formal Concept of all extents
  let fcg = FormalConcept (([],[]), contextExtent totalContext, [])
  -- Add the Formal Concept of all Extents ONLY if a program does not exist that captures this notion
  let l1 = if length (PO.maxima totalConcepts) > 1 then fcg:totalConcepts else totalConcepts
  --let l1 = if doesConceptExtentAlreadyExist fcg totalConcepts then totalConcepts else fcg:totalConcepts
  -- Add the Formal Concept of all Intents ONLY if a program does not exist that captures this notion
  --let l2 = if doesConceptIntentAlreadyExist fcm l1 then l1 else fcm:l1
  let l2 = if length (PO.minima totalConcepts) > 1 then fcm:l1 else l1

  let conceptLattice = mkConceptLattice l2


  let dnNextProgs' = getNextPrograms [getJoin conceptLattice] conceptLattice
  -- reduce down by concepts that are not subsets of others
  let dnNextProgsLabels = map conceptLabel $ filter (\x -> all (\y -> x == y || not (x PO.< y)) dnNextProgs') dnNextProgs'
  let dnNextProgs  = concatMap (\(a,b) -> map (\y -> (y,b)) a) dnNextProgsLabels

  -- TODO, this removed conflicting implications, but there may be a better way to do this
  --let implications = filterCommonPremise (conceptIntent $ getJoin conceptLattice) $ removeConflictingImplications $ deriveImplications (conceptIntent totalContext) $ getTT totalContext
  --let implications = filterCommonPremise (conceptIntent $ getJoin conceptLattice) $ deriveImplications (conceptIntent totalContext) $ getTT totalContext
  --putStrLn $ "Num implications: " ++ (show $ length implications)
  --qq <- stringImplications $ sort $ implications

  -- 0) get object concepts of the goal (knownFormalConcepts)
  -- 1) compute the final knowledge state desired from OCG
  --let finalKS = KnowledgeState goalFormalConcepts (concatMap conceptExtent knownFormalConcepts) (concatMap conceptIntent goalFormalConcepts) --(concatMap conceptExtent knownFormalConcepts, concatMap conceptIntent goalFormalConcepts)
  --putStrLn $ "\n\nFinal Knowledge State: " ++ knowledgeStateToStr finalKS

  -- 2) get the initially known object concepts
  -- 3) use these to build the initial knowledge state
  --let initKS = KnowledgeState knownFormalConcepts (extraKnownProgs ++ map fst knownTaggedPrograms) ((map show extraKnownIntents) ++ concatMap conceptIntent knownFormalConcepts) -- (map fst knownTaggedPrograms, concatMap conceptIntent knownFormalConcepts)
  --putStrLn $ "\nCurrent Knowledge State: " ++ knowledgeStateToStr initKS
  -- 4) store the known concepts as well
    -- model this as a data type for knowledge
  -- 5) compute the direct lower neighbors of this knowledge state (these are the next transitions)
  --let nxtNeighbors = concatMap (getLowerNeighbors conceptLattice) knownFormalConcepts \\ knownFormalConcepts
  --putStrLn $ show nxtNeighbors
  --putStrLn ""
  --let knowledgeSteps = getKnowledgeSteps initKS conceptLattice
  --putStrLn $ show knowledgeSteps
  --putStrLn $ show $ makeUnique $ fg2 knowledgeSteps

  --let prettyLattice = mapL conceptLabel conceptLattice

  -- full detailed formal context as a matrix
  --let (pm,mm,matt) = mkFormalConceptMatrix totalContext
  --putStrLn $ showMatLegend pm
  --putStrLn $ showMatLegend mm
  --putStrLn $ prettyMatrix matt
  let smallMat = mkFormalConceptMatrixSmall totalContext
  --putStrLn $ prettyMatrix smallMat
  --putStrLn "* Exported to CSV"
  exportCSV totalContext smallMat
  -- printout a webpage
  --printWebPage (filter (\(a,_) -> a `elem` extraKnownProgs) cps) kps gps initKS finalKS knowledgeSteps

  dotSpec <- GV.makeDot conceptLattice

  return (dotSpec,dnNextProgs)



-- | Finds the object concept (classification) from a list of classifications (if present)
findObjectConcept :: [FormalConcept] -> Object -> Maybe FormalConcept
findObjectConcept fc o = find (\(FormalConcept ((g,_),_,_)) -> elem o g) fc


-- | simple knowledge state printout
{-
knowledgeStateToStr :: KnowledgeState -> String
knowledgeStateToStr (KnowledgeState ks progs _) = "{"++ join "," progs ++"}\n{" ++ join "," (concatMap conceptIntent ks) ++ "}"
-}

{-
printWebPage :: [(String,String)] -> KnownPrograms -> GoalPrograms -> KnowledgeState -> KnowledgeState -> [KnowledgeStep] -> IO ()
printWebPage eps kps gps (KnowledgeState _ _ attrs') (KnowledgeState _ _ attrs) ls = do
  let dt = "<!DOCTYPE html><html><head><title>Ex. 1</title><script src='site/script.js'></script><link href='site/style.css' type='text/css' rel='stylesheet'/></link></head><div></div><body><div id='main'>"
  let ks = "<div id='wrap'><div class='left'><div>" ++ concatMap t2s kps ++ "<p class='attributes'>(" ++ join ", " (makeUnique attrs') ++ ")</p>" ++ "</div></div><div class='right'><div>" ++ concatMap t2s gps ++ "<p class='attributes'>(" ++ join ", " (makeUnique (attrs \\ attrs')) ++ ")</p></div></div></div>"
  let img = "<img src='R32_Test_1.svg'>"
  --let prgs = "<h2>Step Programs</h2><div>" ++ concatMap sprg eps ++ "</div>"
  --let stps = "<h2>Frontier Steps</h2><div class='steps'>" ++ join "<br/><br/><br/>" (map kstep ls) ++ "</div>"
  let db = "<h3>Here those extra programs on the graph should be shown...</h3></div></body></html>"
  writeFile ("Result.html") $ dt ++ ks ++ img ++ db
  where
    t2s :: (String,String) -> String
    t2s (a,b) = "<h3 class='pn'>" ++ a ++ "</h3><br/><div class='code'>" ++ b ++ "</div>"

    kstep :: KnowledgeStep -> String
    kstep (Step (o,a) ks) = "<div class='step'>Step ({" ++ join "," o ++ "},{" ++ join "," a  ++ "})" ++ "<div class='step-block'>" ++ concatMap kstep ks ++ "</div></div>"
    kstep (Fringe o a) = "<div class='step'>Fringe ({" ++ join "," o ++ "},{" ++ join "," a  ++ "})</div>"

    sprg :: (String,String) -> String
    sprg (a,b) = "<h4>" ++ a ++ "</h4><div class='code'>" ++ b ++ "</div>"
-}


data KnowledgeStep =
  -- a step from a classification to zero or more steps
  Step Label (S.Set KnowledgeStep) |
  -- a fringe step, which ends with a set of unknown programs & concepts that remain to be learned
  Fringe [Object] [Attribute]
    deriving (Show,Ord,Eq)

-- get and report only the total fringe
fringe :: KnowledgeStep -> KnowledgeStep -> KnowledgeStep
fringe fs (Step _ ks) = foldl fringe fs ks
fringe (Fringe o' a'') (Fringe o a) = Fringe (makeUnique $ o++o') (makeUnique $ a++a'')
fringe _ _ = error "Unexpected case for 'fringe'"

fg2 :: [KnowledgeStep] -> [KnowledgeStep]
fg2 [] = []
fg2 ((Step _ ks):ls) = fg2 (S.toList ks) ++ fg2 ls
fg2 (x:ls) = x : fg2 ls


-- Get knowledge steps to learn from
getKnowledgeSteps :: KnowledgeState -> ConceptLattice -> [KnowledgeStep]
getKnowledgeSteps (KnowledgeState ks kprogs kc) cl =
  let mm = PO.maxima ks in
  let ln = concatMap (getLowerNeighbors cl) mm in --concatMap (\un -> map (\y -> (un,y)) (getLowerNeighbors cl un)) ks in
  --let lowerNeighbors = ln in --filter (\(_,q) -> not (q `elem` ks) && (getUpperNeighbors cl q) PO.<= ks) ln in
  map (getKnowledgeSteps' ks) ln
  where
    getKnowledgeSteps' :: [FormalConcept] -> FormalConcept -> KnowledgeStep
    getKnowledgeSteps' ks' x  = let unknownClassifications = conceptLabel x in
                                let unknownConcepts = (uniqueInSameOrder $ snd unknownClassifications) \\ kc in
                                let unknownPrograms = (uniqueInSameOrder $ fst unknownClassifications) \\ kprogs in
                                -- ??? how to fix this relation here
                                case (length unknownConcepts, length unknownPrograms) of
                                  (0,0)  -> Step (conceptLabel x) $ S.fromList $ map (getKnowledgeSteps' (x:ks')) (filter (\q -> (getUpperNeighbors cl q) PO.<= (x:ks')) (getLowerNeighbors cl x))
                                  (_,_)  -> Step (conceptLabel x) $ S.fromList [(Fringe unknownPrograms unknownConcepts)]

convertToCSV :: [[String]] -> String
convertToCSV ls = join "\n" $ map (\q -> join "," (map show q)) ls

addNames :: [Object] -> [[String]] -> [[String]]
addNames [] [] = []
addNames (x:xs) (y:ys) = (x : y) : addNames xs ys
addNames _ _ = error "Unexpected case for 'addNames'"

exportCSV :: FormalContext -> Matrix Mark -> IO ()
exportCSV (g,m,_) mm = do
  let lsts = map (\q -> map show q) (toLists mm)
  let addedNames = addNames g lsts
  writeFile ("export.csv") $ convertToCSV (("":m) : addedNames)

-- determine whether a concept extent already exists
doesConceptExtentAlreadyExist :: FormalConcept -> [FormalConcept] -> Bool
doesConceptExtentAlreadyExist (FormalConcept (_,extent,_)) ls = any (\(FormalConcept (_,g,_)) -> S.fromList g == S.fromList extent) ls

-- determine whether a concept intent already exists
doesConceptIntentAlreadyExist :: FormalConcept -> [FormalConcept] -> Bool
doesConceptIntentAlreadyExist (FormalConcept (_,_,intent)) ls = any (\(FormalConcept (_,_,m)) -> S.fromList m == S.fromList intent) ls

showConcepts :: [FormalConcept] -> String
showConcepts [] = "\n"
showConcepts ((FormalConcept ((s,s'),n,a)):ls) = ("({" ++ join "," s ++ "},{" ++ join "," s' ++ "})") ++ " Extent: " ++ (show n) ++ "\nIntent: " ++ (show a) ++ "\n\n" ++ (showConcepts ls)

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
                                                                               let dupNames = if length dups > 0 then map conceptLabel dups else [] in
                                                                               let fc2 = (FormalConcept (mergeLabels $ n : dupNames, g, m)) in
                                                                               let filt = filter (not . (fcDiffButSameGM fc)) xs in
                                                                               fc2 : combineIC filt ys

-- Produce intersections that do not already exist
--intersectExtents :: [[Object]] -> [Object] -> [[Object]]
--intersectExtents xs y = filter (\z -> not (elem z xs) && z /= []) $ map (intersect y) xs

getConceptsFromContext :: FormalContext -> [FormalConcept]
getConceptsFromContext fc = uniqueInSameOrder $ combineIdenticalConcepts $ mkFormalConceptsFromExtents fc ++ mkFormalConceptsFromIntents fc


-- Convert program in AST to list Terms
astToTerms :: (Data a, Show b, Subsumable b) => (String -> Maybe b) -> (Object,a) -> (Object,[Term])
astToTerms f (name,p) = let terms = mkTermsFromData p in
                        let reducedTerms = S.toList $ S.fromList terms in
                        (name, map show $ subsume $ catMaybes $ map f reducedTerms)

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
                       let objs = if null rset then cm else S.toList $ (S.fromList cm) `S.intersection` rset in
                       uniqueInSameOrder $ objs

-- find formal concepts through object analysis
-- Set A of objs can determine a concept
-- (A'',A')
-- A'' is the extent closure
mkFormalConceptsFromExtents :: FormalContext -> [FormalConcept]
mkFormalConceptsFromExtents fc@(g,_,_) = map (\obj -> FormalConcept (([obj],[]), b' (a' [obj] fc) fc, a' [obj] fc)) g

-- find formal concepts through attribute analysis
-- or set B of attributes determines a concept
-- (B',B'')
-- B'' is the intent closure
mkFormalConceptsFromIntents :: FormalContext -> [FormalConcept]
mkFormalConceptsFromIntents fc@(_,m,_) = map (\atr -> FormalConcept (([],[atr]), b' [atr] fc, a' (b' [atr] fc) fc)) m

-- partially ordered by extents
-- (A1,B1) <= (A2,B2) iff A1 is a subset A2
compareFormalConceptsByExtents :: FormalConcept -> FormalConcept -> Maybe Ordering
compareFormalConceptsByExtents (FormalConcept (_,a,_)) (FormalConcept (_,b,_)) = let sa = S.fromList a
                                                                                     sb = S.fromList b
                                                                                 in
                                                                                    case sa `S.isSubsetOf` sb of
                                                                                      True  -> if sa == sb then Just EQ else Just LT
                                                                                      False -> case sb `S.isSubsetOf` sa of
                                                                                                True  -> Just GT
                                                                                                False -> Nothing

-- a mark can be some mark or no mark
data Mark = Some String | None

instance Show Mark where
  show (Some s) = s
  show None = "."

chooseMatElm :: (Int,Int) -> Mark
chooseMatElm (x,y) = case (x,y) of -- x == 1
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
mkFormalConceptMatrix (g,m,i) = (map (\x -> ("P" ++ show x, g !! (x-1))) [1..(length g)], map (\y -> ("M" ++ show y, m !! (y-1))) [1..(length m)], matrix ((length g) + 1) ((length m) + 1) (\(x,y) -> if x == 1 || y == 1 then chooseMatElm (x,y) else let gx = g !! (x-2) in
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

-- Builds a ConceptLattice from a list of formal concepts
mkConceptLattice :: [FormalConcept] -> ConceptLattice
mkConceptLattice xs = (xs, _mkConceptLattice xs xs)
                      where
                        -- internally constructs the relationships between formal concepts as edges
                        _mkConceptLattice :: [FormalConcept] -> [FormalConcept] -> [(FormalConcept,FormalConcept)]
                        _mkConceptLattice [] _ = []
                        _mkConceptLattice ls bs    = let x = head $ PO.maxima ls in -- get one of the maxima to work with
                                                     -- compute direct lower neighbors
                                                     let subConcepts = filter (\y -> y PO.< x) bs in -- find all subconcepts of our picked maxima
                                                     -- get the most general subconcepts that are direct neighbors of concept 'x' (i.e, the maxima)
                                                     let neighbors = PO.maxima subConcepts in
                                                     -- creates edges from x -> neighbors
                                                     let edges = map (\z -> (x,z)) neighbors in
                                                     -- remove our plucked maxima from the running list
                                                     let subList = ls \\ [x] in
                                                     -- add edges, and continue
                                                     edges ++ (_mkConceptLattice subList bs)

--
-- TODO attribute implication work
--
getTT :: FormalContext -> [[Attribute]]
getTT (g,_,i) = reverse $ map (getAttrList i) g
                             where
                               getAttrList :: [(Object,Attribute)] -> Object -> [Attribute]
                               getAttrList i' o = map snd $ filter (\(a,_) -> a == o) i'

deriveImplications :: [Attribute] -> [[Attribute]] -> [Implication]
deriveImplications [] _ = []
deriveImplications (x:xs) ys = let x1 = map (\\ [x]) (filter (\yy -> elem x yy) ys) in -- retrieve all rows that have 'x' present (and remove 'x' as a possible premise)
                               let inter = foldl intersect (x1 !! 0) (tail x1) in -- compute the intersection of all rows that have 'x' present
                               (inter,[x]) : (deriveImplications xs ys) -- use the intersection of all rows as an implication for attribute 'x'

-- | Remove conflicting implications
-- TODO This rule is the problem, can probably re worked to better capture what I want it to do
removeConflictingImplications :: [Implication] -> [Implication]
removeConflictingImplications ls = filter (\(_,c) -> length c == 1) (_mi ls ls) -- only allow implications to pass that are valid, i.e. unambiguous
                       where
                        _mi :: [Implication] -> [Implication] -> [Implication]
                        _mi [] _ = []
                        _mi ((p,_):xs) ys = let filt = filter (\(p',_) -> p == p') ys in
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
