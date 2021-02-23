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
import qualified Data.PartialOrd as PO
import qualified P2021.GraphViz as GV

import qualified Debug.Trace as DT

--
-- BoGL Stuff
--
import P2021.Bogl_Specifics

import Language.Syntax (Game)
import Text.Parsec.Pos
--import qualified Data.Set as DS
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

conceptLabel :: FormalConcept -> Label
conceptLabel (FormalConcept (n,_,_)) = n

conceptIntent :: FormalConcept -> [Attribute]
conceptIntent (FormalConcept (_,_,m)) = m

conceptExtent :: FormalConcept -> [Object]
conceptExtent (FormalConcept (_,g,_)) = g

instance PO.PartialOrd FormalConcept where
  --(<=) a b = let r = compareFormalConceptsByExtents a b in r /= GT
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
  compare a b = compareFormalConceptsByExtents a b


instance GV.GraphVizable FormalConcept where
  node (FormalConcept (([],[]),_,_))  = [("label","")]
  node (FormalConcept ((g,m),_,_))    = [("label",join ", " g),("fontcolor","purple"),("fontname","Helvetica")]
  edge (FormalConcept ((_,a),_,_)) (FormalConcept ((_,b),_,_)) = let d = (b \\ a) in
                                                                 if length d > 0 then
                                                                   [("label",join ", " d),("fontname","Helvetica")]
                                                                 else
                                                                   []


strConcept :: FormalConcept -> String
strConcept (FormalConcept (_,g,m)) = "({" ++ join "," g ++ "},{" ++ join "," m ++ "})"

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
data FCA a b = FCA OrderBy ConceptsBy (ParsingFunction a) (ConceptMapping b) KnownPrograms GoalPrograms CoursePrograms [Object] [b]

type Lattice a = ([a],[(a,a)])
type ConceptLattice = ([FormalConcept],[(FormalConcept,FormalConcept)])

getAllNodes :: ConceptLattice -> [FormalConcept]
getAllNodes (nodes,_) = nodes

-- Return the lower neighbors of this node in the concept lattice, if any
getLowerNeighbors :: ConceptLattice -> FormalConcept -> [FormalConcept]
getLowerNeighbors (_,edges) fc = map snd $ filter (\(i,o) -> i PO.== fc) edges

-- Return the upper neighbors of this node in the concept lattice, if any
getUpperNeighbors :: ConceptLattice -> FormalConcept -> [FormalConcept]
getUpperNeighbors (_,edges) fc = map fst $ filter (\(i,o) -> o PO.== fc) edges

-- returns supremum of the concept lattice (Top most node, most general, concept of all objects)
getJoin :: ConceptLattice -> FormalConcept
getJoin (nodes,_) = head $ PO.maxima nodes

-- returns the infimum of the concept lattice (Bottom most node, most specific, concept of all attributes)
getMeet :: ConceptLattice -> FormalConcept
getMeet (nodes,_) = head $ PO.minima nodes

mapL :: (a -> b) -> Lattice a -> Lattice b
mapL f (nodes,edges) = (map f nodes, map (\(a,b) -> (f a, f b)) edges)

--graphConceptLattice :: String -> ConceptLattice -> IO ()
--graphConceptLattice name cl = GV.writeLattice name cl

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

-- knowledge state is modeled with
-- [KnownClassifications] [KnownPrograms] [KnownAttributes]
data KnowledgeState = KnowledgeState [FormalConcept] [Object] [Attribute]
  deriving Show

-- This is for TESTING, the actual setup will not be an IO monad (essentially a prototyping sandbox)
-- run the analysis, taking an FCA analysis instance
-- Takes known programs, goal programs, and course programs (programs available in the course)
r32 :: (Data a, Show a, Subsumable b, Show b) => FCA a b -> IO ()
r32 (FCA ob cb programParser conceptMapping kps gps cps extraKnownProgs extraKnownIntents) = do
  -- parse the programs that are understood
  let parsedKPS = programParser kps
  -- parse the goal programs that are desired to be understood
  let parsedGPS = programParser gps
  -- parse course programs that are available as extents
  let parsedCPS = programParser cps

  -- extract terms for the known list of programs, preserving names
  let knownTaggedPrograms = (map (astToTerms conceptMapping) parsedKPS)
  -- extract terms for the goal list of programs, preserving names
  let goalTaggedPrograms = map (astToTerms conceptMapping) parsedGPS
  -- extract terms for the course list of programs, preserving names
  let courseTaggedPrograms = map (astToTerms conceptMapping) parsedCPS

  -- produce total context
  -- from known & goal programs, we want to create an 'intent of interest'
  -- we will use this to reduce the attributes we have in our graph to only those we care about (between goal & known inclusively)
  let intentOfInterest = uniqueInSameOrder $ concatMap snd knownTaggedPrograms ++ concatMap snd goalTaggedPrograms
  let filteredCourseProgs = filter (\(_,b) -> S.fromList b `S.isSubsetOf` S.fromList intentOfInterest) courseTaggedPrograms
  let (g,m,i) = mkFormalContext (uniqueInSameOrder' $ knownTaggedPrograms ++ goalTaggedPrograms ++ filteredCourseProgs)
  let totalContext = (g, filter (`elem` intentOfInterest) m, filter (\(_,b) -> b `elem` intentOfInterest) i)

  -- produce total concepts from total context
  let totalConcepts' = getConceptsFromContext cb totalContext
  -- get known formal concepts, factoring in those that have been implicitly indicated by the programs & attributes added so far while learning
  let knownFormalConcepts = (mkFormalConceptsFromSubExtents totalContext (map fst knownTaggedPrograms)) ++ (filter (\(FormalConcept ((g,m),_,_)) -> S.fromList g `S.isSubsetOf` S.fromList extraKnownProgs && S.fromList m `S.isSubsetOf` S.fromList (map show extraKnownIntents) && (not $ S.null $ S.fromList g) && (not $ S.null $ S.fromList m)) totalConcepts')
  -- goal is fixed to what programs are given in the goal
  let goalFormalConcepts = mkFormalConceptsFromSubExtents totalContext (map fst goalTaggedPrograms)
  -- Apply Bounding concepts to get sub-lattice to work with
  let boundingConcepts = mkFormalConceptsFromSubExtents totalContext ((map fst knownTaggedPrograms) ++ (map fst goalTaggedPrograms))
  let totalConcepts = case (length knownFormalConcepts > 0, length goalFormalConcepts > 0) of
                        -- no bounds
                        (False,False) -> totalConcepts'
                        -- upper & lower bounds
                        (True,True)   -> filter (\x -> any (x PO.>=) (PO.minima boundingConcepts) && any (x PO.<=) (PO.maxima boundingConcepts)) totalConcepts'
                        -- lower bound, we only want programs more specific than this
                        (True,False)  -> filter (\x -> any (x PO.<=) (PO.minima boundingConcepts)) totalConcepts'
                        -- upper bound, we only want programs more general than this
                        (False,True)  -> filter (\x -> any (x PO.>=) (PO.maxima boundingConcepts)) totalConcepts'

  putStrLn $ "Num Course Progs: " ++ (show $ length cps)
  putStrLn $ "Num Course Objects: " ++ (show $ length ((\(x,_,_) -> x) totalContext))
  putStrLn $ "Num Course attributes: " ++ (show $ length ((\(_,x,_) -> x) totalContext))
  putStrLn $ "Num Formal Concepts (Program,Attribute set pairs): " ++ (show $ length totalConcepts)
  --putStrLn $ showConcepts (sortBy (getConceptOrdering ob) totalConcepts)
  putStrLn $ showConcepts totalConcepts

  -- report Known Concepts
  let knownConcepts = S.toList $ S.fromList (concatMap conceptIntent knownFormalConcepts)
  putStrLn $ "Known Concepts: " ++ show knownConcepts

  -- report Goal concepts (minus Known), these represent all concepts in the reduced set
  let goalConcepts = (S.toList $ S.fromList (concatMap conceptIntent goalFormalConcepts)) \\ knownConcepts
  putStrLn $ "Goal Concepts: " ++ show goalConcepts

  -- create Formal Concept of all intents
  let m' = FormalConcept (([],[]), [], (\(_,m,_) -> m) totalContext)
  -- create Formal Concept of all extents
  let g' = FormalConcept (([],[]), (\(a,_,_) -> a) totalContext, [])
  -- Add the Formal Concept of all Extents ONLY if a program does not exist that captures this notion
  let l1 = if doesConceptExtentAlreadyExist g' totalConcepts then totalConcepts else totalConcepts ++ [g']
  -- Add the Formal Concept of all Intents ONLY if a program does not exist that captures this notion
  let l2 = if doesConceptIntentAlreadyExist m' l1 then l1 else l1 ++ [m']

  let conceptLattice = mkConceptLattice l2


  -- TODO, this removed conflicting implications, but there may be a better way to do this
  --let implications = filterCommonPremise (conceptIntent $ getJoin conceptLattice) $ removeConflictingImplications $ deriveImplications (fcM totalContext) $ getTT totalContext
  let implications = filterCommonPremise (conceptIntent $ getJoin conceptLattice) $ deriveImplications (fcM totalContext) $ getTT totalContext
  --putStrLn $ "Num implications: " ++ (show $ length implications)
  --qq <- stringImplications $ sort $ implications

  -- 0) get object concepts of the goal (knownFormalConcepts)
  -- 1) compute the final knowledge state desired from OCG
  let finalKS = KnowledgeState goalFormalConcepts (concatMap conceptExtent knownFormalConcepts) (concatMap conceptIntent goalFormalConcepts) --(concatMap conceptExtent knownFormalConcepts, concatMap conceptIntent goalFormalConcepts)
  putStrLn $ "\n\nFinal Knowledge State: " ++ knowledgeStateToStr finalKS

  -- 2) get the initially known object concepts
  -- 3) use these to build the initial knowledge state
  let initKS = KnowledgeState knownFormalConcepts (extraKnownProgs ++ map fst knownTaggedPrograms) ((map show extraKnownIntents) ++ concatMap conceptIntent knownFormalConcepts) -- (map fst knownTaggedPrograms, concatMap conceptIntent knownFormalConcepts)
  putStrLn $ "\nCurrent Knowledge State: " ++ knowledgeStateToStr initKS
  -- 4) store the known concepts as well
    -- model this as a data type for knowledge
  -- 5) compute the direct lower neighbors of this knowledge state (these are the next transitions)
  --let nxtNeighbors = concatMap (getLowerNeighbors conceptLattice) knownFormalConcepts \\ knownFormalConcepts
  --putStrLn $ show nxtNeighbors
  putStrLn ""
  --putStrLn $ exploreNeighborhoodAuto "" finalKS initKS conceptLattice
  let knowledgeSteps = getKnowledgeSteps initKS conceptLattice
  putStrLn $ show knowledgeSteps
  putStrLn $ show $ makeUnique $ fg2 knowledgeSteps
  --let zz = explainNeighbors initKS finalKS nxtNeighbors
  -- 6) Present the new attributes of the INTENT of the neighbor (or all of them)
  -- 7) Explore to all options automatically, until we are done, and then continue (basically a breadth-first search)

  --let prettyLattice = mapL conceptLabel conceptLattice
  --graphConceptLattice "R32_Test_1" conceptLattice
  GV.makeDGraph "R23_Test_1" conceptLattice

  -- TODO full detailed formal context as a matrix
  let (pm,mm,matt) = mkFormalConceptMatrix totalContext
  --putStrLn $ showMatLegend pm
  --putStrLn $ showMatLegend mm
  --putStrLn $ prettyMatrix matt
  let smallMat = mkFormalConceptMatrixSmall totalContext
  --putStrLn $ prettyMatrix smallMat
  --putStrLn "* Exported to CSV"
  exportCSV totalContext smallMat

  printWebPage (filter (\(a,b) -> a `elem` extraKnownProgs) cps) kps gps initKS finalKS knowledgeSteps



-- simple knowledge state printout
knowledgeStateToStr :: KnowledgeState -> String
knowledgeStateToStr (KnowledgeState ks progs _) = "{"++ join "," progs ++"}\n{" ++ join "," (concatMap conceptIntent ks) ++ "}"


printWebPage :: [(String,String)] -> KnownPrograms -> GoalPrograms -> KnowledgeState -> KnowledgeState -> [KnowledgeStep] -> IO ()
printWebPage eps kps gps fks@(KnowledgeState ks' progs' attrs') kks@(KnowledgeState ks progs attrs) ls = do
  let dt = "<!DOCTYPE html><html><head><title>Ex. 1</title><script src='site/script.js'></script><link href='site/style.css' type='text/css' rel='stylesheet'/></link></head><div></div><body><div id='main'>"
  let ks = "<div>" ++ concatMap t2s kps ++ "<p class='attributes'>(" ++ join ", " (makeUnique attrs') ++ ")</p>" ++ "<br/>" ++ concatMap t2s gps ++ "<p class='attributes'>(" ++ join ", " (makeUnique (attrs \\ attrs')) ++ ")</p></div>"
  let img = "<h2>Lattice from Known to Goal</h3><img src='R32_Test_1.png'>"
  let prgs = "<h2>Step Programs</h2><div>" ++ concatMap sprg eps ++ "</div>"
  let stps = "<h2>Frontier Steps</h2><div class='steps'>" ++ join "<br/><br/><br/>" (map kstep ls) ++ "</div>"
  let db = "</div></body></html>"
  writeFile ("Result.html") $ dt ++ ks ++ img ++ prgs ++ stps ++ db
  where
    t2s :: (String,String) -> String
    t2s (a,b) = "<h3 class='pn'>" ++ a ++ "</h3><br/><div class='code'>" ++ b ++ "</div>"

    kstep :: KnowledgeStep -> String
    kstep (Step (o,a) ks) = "<div class='step'>Step ({" ++ join "," o ++ "},{" ++ join "," a  ++ "})" ++ "<div class='step-block'>" ++ concatMap kstep ks ++ "</div></div>"
    kstep (Fringe o a) = "<div class='step'>Fringe ({" ++ join "," o ++ "},{" ++ join "," a  ++ "})</div>"
    --t3s :: (String,String,String) -> String
    --t2s (a,b,c) = "<h3 class='pn'>" ++ a ++ "</h3><p class='attributes'>(" ++ c ++ ")</p><br/><div class='code'>" ++ b ++ "</div>"

    sprg :: (String,String) -> String
    sprg (a,b) = "<h4>" ++ a ++ "</h4><div class='code'>" ++ b ++ "</div>"



data KnowledgeStep =
  -- a step from a classification to zero or more steps
  Step Label (S.Set KnowledgeStep) |
  -- a fringe step, which ends with a set of unknown programs & concepts that remain to be learned
  Fringe [Object] [Attribute]
    deriving (Show,Ord,Eq)

-- get and report only the total fringe
fringe :: KnowledgeStep -> KnowledgeStep -> KnowledgeStep
fringe fs (Step ls ks) = foldl fringe fs ks
fringe (Fringe o' a') (Fringe o a) = Fringe (makeUnique $ o++o') (makeUnique $ a++a')

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


--explainUnknownProgram :: String -> Object -> String
--explainUnknownProgram b p = b ++ "- p " ++ p ++ "\n"

--explainUnknownConcept :: String -> Attribute -> String
--explainUnknownConcept b c = b ++ "- c " ++ c ++ "\n"

-- Change this to a BREADTH first search
{-
exploreNeighborhoodAuto :: String -> KnowledgeState -> KnowledgeState -> ConceptLattice -> String
exploreNeighborhoodAuto s fks@(KnowledgeState fs fprogs fc) kks@(KnowledgeState ks kprogs kc) cl = -- get the lower neighbors
                                                                                         let ln = filter (\(_,q) -> not $ q `elem` ks) $ concatMap (\un -> map (\y -> (un,y)) (getLowerNeighbors cl un)) ks in
                                                                                         if length ln > 0 then
                                                                                           let unknownPrograms = (uniqueInSameOrder $ (concatMap (\(from,to) -> conceptExtent from \\ conceptExtent to) ln)) \\ kprogs in
                                                                                           let unknownConcepts = (uniqueInSameOrder $ (concatMap (\(from,to) -> conceptIntent from) ln)) \\ kc in
                                                                                           case (length unknownConcepts, length unknownPrograms) of
                                                                                             -- nothing to introduce for these neighbors, so add a neighbor and continue
                                                                                             (0,0)  -> exploreNeighborhoodAuto (s++"  ") fks (KnowledgeState ((snd $ head ln):ks) kprogs kc) cl
                                                                                             -- program to introduce
                                                                                             (0,y)  -> explainUnknownProgram s (head unknownPrograms) ++ exploreNeighborhoodAuto s fks (KnowledgeState ks ((head unknownPrograms):kprogs) kc) cl
                                                                                             -- syntactic concept to introduce
                                                                                             (x,_)  -> explainUnknownConcept s (head unknownConcepts) ++ exploreNeighborhoodAuto s fks (KnowledgeState ks kprogs ((head unknownConcepts):kc)) cl
                                                                                         else
                                                                                           -- no more neighbors to explore, but verify we understand the goal by checking w/ upper neighbors
                                                                                           let ln = concatMap (\un -> map (\y -> (y,un)) (getUpperNeighbors cl un)) fs in
                                                                                           let unknownPrograms = fprogs \\ kprogs in
                                                                                           let unknownConcepts = fc \\ kc in
                                                                                           case (length unknownConcepts, length unknownPrograms) of
                                                                                             -- done!
                                                                                             (0,0)  -> s ++ "Goal Set Met "
                                                                                             -- program to introduce
                                                                                             (0,y)  -> explainUnknownProgram s (head unknownPrograms) ++ exploreNeighborhoodAuto s fks (KnowledgeState ks ((head unknownPrograms):kprogs) kc) cl
                                                                                             -- syntactic concept to introduce
                                                                                             (x,_)  -> explainUnknownConcept s (head unknownConcepts) ++ exploreNeighborhoodAuto s fks (KnowledgeState ks kprogs ((head unknownConcepts):kc)) cl
-}

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

getConceptOrdering :: OrderBy -> (FormalConcept -> FormalConcept -> Maybe Ordering)
getConceptOrdering OrderByIntents = compareFormalConceptsByIntents
getConceptOrdering OrderByExtents = compareFormalConceptsByExtents
--getConceptOrdering OrderByAll = undefined -- since this is not a bool how do we order these?????

showConcepts :: [FormalConcept] -> String
showConcepts [] = "\n"
showConcepts (fc@(FormalConcept ((s,s'),n,a)):ls) = ("({" ++ join "," s ++ "},{" ++ join "," s' ++ "})") ++ " Extent: " ++ (show n) ++ "\nIntent: " ++ (show a) ++ "\n\n" ++ (showConcepts ls)

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
astToTerms :: (Data a, Show b, Subsumable b) => (String -> Maybe b) -> (Object,a) -> (Object,[Term])
astToTerms f (name,p) = (name, uniqueInSameOrder $ map show $ subsume $ catMaybes $ map f $ mkTermsFromData p)

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
compareFormalConceptsByExtents :: FormalConcept -> FormalConcept -> Maybe Ordering
compareFormalConceptsByExtents (FormalConcept (_,a,_)) (FormalConcept (_,b,_)) = let sa = S.fromList a
                                                                                     sb = S.fromList b
                                                                                 in
                                                                                    case sa `S.isSubsetOf` sb of
                                                                                      True  -> if sa == sb then Just EQ else Just LT
                                                                                      False -> case sb `S.isSubsetOf` sa of
                                                                                                True  -> Just GT
                                                                                                False -> Nothing

-- partially ordered by intents (these 2 should be equivalent ways of doing this)
-- (A1,B1) <= (A2,B2) iff B2 is a subset B1
compareFormalConceptsByIntents :: FormalConcept -> FormalConcept -> Maybe Ordering
compareFormalConceptsByIntents (FormalConcept (_,_,a)) (FormalConcept (_,_,b)) = let sa = S.fromList a
                                                                                     sb = S.fromList b
                                                                                 in
                                                                                    case sb `S.isSubsetOf` sa of
                                                                                      True  -> if sb == sa then Just EQ else Just LT
                                                                                      False -> case sa `S.isSubsetOf` sb of
                                                                                                True  -> Just GT
                                                                                                False -> Nothing

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

-- Builds a ConceptLattice from a list of formal concepts
mkConceptLattice :: [FormalConcept] -> ConceptLattice
mkConceptLattice xs = (xs, _mkConceptLattice xs xs)

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
