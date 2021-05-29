{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
--
-- ProgramConceptClassifier implementation
--
module ProgramConceptClassifier where

import Data.Data
import General -- general utilities
import qualified Data.Set as S
import Data.List
import Data.Maybe
import qualified Data.PartialOrd as PO
import qualified GraphViz as GV

-- | Terms are names in this context, both terminal & nonterminal
type Term = String

-- Object from FCA
type Object = String

-- Attribute of an Object, from FCA
type Attribute = String

-- attribute pairs to help describe nodes in GraphViz
type AttributePairs = [(String,String)]

-- A label for a classification corresponds
-- the objects & attributes that map directly to this closure
-- i.e. the single objects & attributes (sets of 1) that map to this
type Label = ([Object],[Attribute],AttributePairs)

lfst :: Label -> [Object]
lfst (a,_,_) = a

-- Pair of sets, (A subset G, B subset M)
-- A is the extent: set of all objs that have all the attributes in B
  -- For all 'a' in G. 'a' in A iff (For all 'b' in B. b(a))
-- B is the intent: set of all attributes that match to all objects in A
  -- For all 'b' in M. 'b' in B iff (For all 'a' in A. b(a))
-- Also has a name, for easy id
data Classification = Classification (Label,[Object],[Attribute])
  deriving (Eq,Show)

classificationLabel :: Classification -> Label
classificationLabel (Classification (n,_,_)) = n

classificationIntent :: Classification -> [Attribute]
classificationIntent (Classification (_,_,m)) = m

classificationExtent :: Classification -> [Object]
classificationExtent (Classification (_,g,_)) = g

instance PO.PartialOrd Classification where
  (<=) a b = case compareClassificationsByExtents a b of
              Just r  -> r /= GT
              Nothing -> False
  (==) a b = case compareClassificationsByExtents a b of
              Just r  -> r == EQ
              _       -> False
  (<) a b = case compareClassificationsByExtents a b of
              Just r  -> r == LT
              Nothing -> False

  (>=) a b = case compareClassificationsByExtents a b of
              Just r  -> r /= LT
              Nothing -> False
  (>) a b = case compareClassificationsByExtents a b of
              Just r  -> r == GT
              Nothing -> False

underscoreReplace :: String -> String
underscoreReplace = map (\x -> if x == '_' then ' ' else x)

-- | Full purple for the graph
purple :: String
purple = "#770077"

-- | Faded purple for the graph
fadedPurple :: String
fadedPurple = "#77007733"

instance GV.GraphVizable Classification where
  node (Classification (([],[],a),_,_))  = [("label","")] ++ a
  node (Classification (([],_,a),_,_))   = [("shape","point")] ++ a -- point marks
  node (Classification ((g,_,a),_,_))    = case elem "Known" g of
    True  -> [
      ("label",join ", " (map underscoreReplace g)),
      ("fontcolor","white"),
      ("style","filled"),
      ("fillcolor",purple),
      ("fontname","Helvetica")] ++ a
    False -> [("label",join ", " (map underscoreReplace g)),("fontname","Helvetica")] ++ a
  edge (Classification ((_,a,_),_,_)) (Classification ((_,b,atrs'),_,_)) =
    let d = (b \\ a) in
    if length d > 0 then
      case elem ("fontcolor",fadedPurple) atrs' of
        True  -> [("label",join ", " (map underscoreReplace d)),("fontname","Helvetica"),("fontcolor","#ff8c0033")] ++ atrs' -- fade it
        False -> [("label",join ", " (map underscoreReplace d)),("fontname","Helvetica"),("fontcolor","darkorange")] ++ atrs' -- normal color
    else
      [] ++ atrs'

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

-- data type used for performing analysis, programs that are generically mappable to classifications
data MappablePrograms a b = MappablePrograms (ConceptMapping b) (KnownPrograms a) (GoalPrograms a) (CoursePrograms a)

type ClassificationLattice = ([Classification],[(Classification,Classification)])

-- | Return the lower neighbors of this node in the classification lattice, if any
getLowerNeighbors :: ClassificationLattice -> Classification -> [Classification]
getLowerNeighbors (_,edges) fc = map snd $ filter (\(i,_) -> i PO.== fc) edges

-- | Return the upper neighbors of this node in the classification lattice, if any
getUpperNeighbors :: ClassificationLattice -> Classification -> [Classification]
getUpperNeighbors (_,edges) fc = map fst $ filter (\(_,o) -> o PO.== fc) edges

-- | Get all upper neighbors from a classification
getAllUpperNeighbors :: [Classification] -> Classification -> [Classification]
getAllUpperNeighbors xs fc =
  let un = PO.maxima $ filter (\y -> y PO.> fc) xs in
  case un of
    [] -> []
    _  -> concatMap (getAllUpperNeighbors xs) un

-- | Get all lower neighbors from a classification
getLowerNeighbors' :: [Classification] -> Classification -> [Classification]
getLowerNeighbors' xs fc = PO.maxima $ filter (\y -> y PO.< fc) xs

-- returns supremum of the pc lattice (Top most node, most general, classification of all objects)
getJoin :: ClassificationLattice -> Classification
getJoin (nodes,_) = head $ PO.maxima nodes

-- returns the infimum of the pc lattice (Bottom most node, most specific, classification of all attributes)
getMeet :: ClassificationLattice -> Classification
getMeet (nodes,_) = head $ PO.minima nodes

data Wrapper = Wrap String

instance Show Wrapper where
  show (Wrap s) = s

asIsConceptMapping :: String -> Maybe Wrapper
asIsConceptMapping s = Just (Wrap s)

-- knowledge state is modeled with
data KnowledgeState = KnowledgeState [Classification] [Object] [Attribute]
  deriving Show

-- | Gets the next programs, by classifications that don't have empty object labels
getNextPrograms :: [Classification] -> ClassificationLattice -> [Classification]
getNextPrograms [] _  = []
getNextPrograms ks cl =
  let lf = filter (\x -> not $ null (lfst $ classificationLabel x)) ks in
  let le = filter (\x -> null (lfst $ classificationLabel x)) ks in
  lf ++ (getNextPrograms (concatMap (getLowerNeighbors cl) le) cl)

-- | Return list of pairs of programs (objects) & their unique terms
getUniqueTerms :: (Data a, Show a, Subsumable b, Show b) => ConceptMapping b -> [(String,a)] -> [(Object,[Term])]
getUniqueTerms f p = S.toList $ S.fromList $ map (dataToTerms f) p

-- | Run the analysis, taking an instance of mappable programs to work on
-- Takes known programs, goal programs, and course programs (programs available in the course)
-- returns a Dot specification & a list of programs in the outer 'fringe'
analyze :: (Data a, Show a, Subsumable b, Show b) => MappablePrograms a b -> (String,[(String,[String])])
analyze (MappablePrograms conceptMapping kps gps cps) = do
  -- do notation abuse, I am aware of this...could be written better
  -- extract terms for the these program lists, preserving names
  -- ### STEP 1: extract unique terms from AST, (program,[term])
  let [knownTaggedPrograms,goalTaggedPrograms,courseTaggedPrograms] = map (getUniqueTerms conceptMapping) [kps,gps,cps]

  -- ### STEP 2: produce formal context
  let totalContext = produceContext knownTaggedPrograms goalTaggedPrograms courseTaggedPrograms

  -- ### STEP 3: get classifications
  -- produce total classification from total context
  let totalClassifications' = getClassificationsFromContext totalContext
  -- get known classifications, factoring in those that have been implicitly indicated by the programs & attributes added so far while learning
  let knownClassifications = catMaybes $ map (findProgramClassification totalClassifications') (map fst knownTaggedPrograms)
  let goalClassifications = catMaybes $ map (findProgramClassification totalClassifications') (map fst goalTaggedPrograms)
  -- Apply Bounding classifications to get sub-lattice to work with
  let boundingClassifications = catMaybes $ map (findProgramClassification totalClassifications') ((map fst knownTaggedPrograms) ++ (map fst goalTaggedPrograms))

  let upKnownNeigh = concatMap (getAllUpperNeighbors totalClassifications') knownClassifications
  let upGoalNeigh = concatMap (getAllUpperNeighbors totalClassifications') goalClassifications

  let totalClassifications = case (length knownClassifications > 0, length goalClassifications > 0) of
                              -- no bounds
                              (False,False) -> totalClassifications'
                              -- i.e. the PO approach seems to work there
                              -- upper & lower bounds, we want programs that are more specific than the most general program
                              -- and are more general than the most specific program (bounded set)
                              (True,True)   -> filter (\x -> (not $ elem x upKnownNeigh || elem x knownClassifications) || (elem x upGoalNeigh || elem x goalClassifications)) totalClassifications'
                              -- lower bound, we only want programs more specific than this set (smaller extent)
                              (True,False)  -> filter (\x -> not $ elem x upKnownNeigh || elem x knownClassifications) totalClassifications'
                              -- upper bound, we only want programs more general than this set (larger extent)
                              (False,True)  -> filter (\x -> any (x PO.>=) (PO.minima boundingClassifications)) totalClassifications'

  -- ### STEP 4:  produce classification lattice
  -- create classification of all intents
  let fcm = Classification (([],[],[]), [], contextIntent totalContext)
  -- create classification of all extents
  let fcg = Classification (([],[],[]), contextExtent totalContext, [])
  -- Add the classification of all Extents ONLY if a program does not exist that captures this notion
  let l1 = if length (PO.maxima totalClassifications) > 1 then fcg:totalClassifications else totalClassifications
  -- Add the classification of all Intents ONLY if a program does not exist that captures this notion
  let l2 = if length (PO.minima totalClassifications) > 1 then fcm:l1 else l1

  let pcLattice = mkProgramClassificationLattice l2

  -- combining from the join & the known program, because they are not always at the same position, and the fringe extends from the join AND the known
  -- this accounts for programs that directly follow from what is know, AND programs that are not connected, but require a step to get started in
  -- these may overlap, and so they are filtered afterwards
  -- HOWEVER, we must remove the 'known' program, otherwise it will cause all those items that are explorable below it to be subsumed away by accident
  let dnNextProgs' = filter (\x -> not $ elem x knownClassifications) $ (getNextPrograms (getLowerNeighbors pcLattice (getJoin pcLattice)) pcLattice) ++ (getNextPrograms (concatMap (getLowerNeighbors pcLattice) knownClassifications) pcLattice)
  -- reduce down by classifications that are not subsets of others
  let dnNextProgsReduced = filter (\x -> all (\y -> x == y || not (x PO.< y)) dnNextProgs') dnNextProgs'
  let dnNextProgs  = concatMap (\(a,b,_) -> map (\y -> (y,b)) a) (map classificationLabel dnNextProgsReduced)

  -- remap ALL classifications, if any are a subset of the fringe programs, add full color (including the fringe)
  -- any that are not, make them partial colors
  let l2' = map (\x@(Classification ((lg,lm,la),g,m)) -> case (any (x PO.>=) dnNextProgsReduced) of
                                                          True  -> Classification ((lg,lm,("fontcolor","#770077"):("color","#000000"):la),g,m) -- full color (reachable/known)
                                                          False -> Classification ((lg,lm,("fontcolor","#77007733"):("color","#00000033"):la),g,m)) l2 -- faded color (not yet reached/unknown)

  -- produce pair of dot spect & fringe
  -- dot spec needs a visually lattice, using the annotated elements (setting faded coloring on elements past the fringe) (using l2')
  (GV.makeDot $ mkProgramClassificationLattice l2', dnNextProgs)


-- | Produce a formal context
produceContext :: [(Object,[Term])] -> [(Object,[Term])] -> [(Object,[Term])] -> FormalContext
produceContext knownTaggedPrograms goalTaggedPrograms courseTaggedPrograms =
  -- from known & goal programs, we want to create an 'intent of interest'
  -- we will use this to reduce the attributes we have in our graph to only those we care about (between goal & known inclusively)
  -- filter by lower intent, removing any programs that are less any other program we 'know'
  let fcp1 = case knownTaggedPrograms of
              [] -> courseTaggedPrograms
              _  -> filter (\(_,b) -> all (\z -> (not $ S.fromList b `S.isSubsetOf` S.fromList z)) (map snd knownTaggedPrograms)) courseTaggedPrograms in
  -- filter by upper intent, including any program that is a subset of any goal program
  let filteredCourseProgs = case goalTaggedPrograms of
                              [] -> fcp1
                              _  -> filter (\(_,b) -> any (\z -> S.fromList b `S.isSubsetOf` S.fromList z) (map snd goalTaggedPrograms)) fcp1 in
  -- generate decomposed formal context
  let (g',m',i') = mkFormalContext (uniqueInSameOrder $ knownTaggedPrograms ++ goalTaggedPrograms ++ filteredCourseProgs) in
  -- only reduce the context against the upper intent (include only those programs & concepts that are within that scope)
  -- removes extraneous detail that is unrelated to this query
  case (uniqueInSameOrder $ concatMap snd goalTaggedPrograms) of
      []  -> (g',m',i')
      ul  -> (g', filter (`elem` ul) m', filter (\(_,b) -> b `elem` ul) i')


-- | Finds the program classification from a list of classifications (if present)
findProgramClassification :: [Classification] -> Object -> Maybe Classification
findProgramClassification fc o = find (\(Classification ((g,_,_),_,_)) -> elem o g) fc

showClassifications :: [Classification] -> String
showClassifications [] = "\n"
showClassifications ((Classification ((s,s',_),n,a)):ls) = ("({" ++ join "," s ++ "},{" ++ join "," s' ++ "})") ++ " Extent: " ++ (show n) ++ "\nIntent: " ++ (show a) ++ "\n\n" ++ (showClassifications ls)

fcDiffButSameGM :: Classification -> Classification -> Bool
fcDiffButSameGM (Classification (n1,g1,m1)) (Classification (n2,g2,m2)) = (S.fromList g1) == (S.fromList g2) && (S.fromList m1) == (S.fromList m2) && n1 /= n2

mergeLabels :: [Label] -> Label
mergeLabels ls = foldl (\(g,m,_) (sg,sm,_) -> (sg++g, sm++m, [])) ([],[],[]) ls

-- | Combines identical classifications with the same extent & intent, by joining their names & tossing one
combineIdenticalClassifications :: [Classification] -> [Classification]
combineIdenticalClassifications ls = combineIC ls ls
  where
    combineIC :: [Classification] -> [Classification] -> [Classification]
    combineIC [] _ = []
    combineIC (fc@(Classification (n,g,m)):xs) ys = let dups = filter (fcDiffButSameGM fc) ys in
                                                   let dupNames = if length dups > 0 then map classificationLabel dups else [] in
                                                   let fc2 = (Classification (mergeLabels $ n : dupNames, g, m)) in
                                                   let filt = filter (not . (fcDiffButSameGM fc)) xs in
                                                   fc2 : combineIC filt ys

getClassificationsFromContext :: FormalContext -> [Classification]
getClassificationsFromContext fc = uniqueInSameOrder $ combineIdenticalClassifications $ mkClassificationsFromExtents fc ++ mkClassificationsFromIntents fc


-- | Converts an instance of data to a list of Terms
dataToTerms :: (Data a, Show b, Subsumable b) => (String -> Maybe b) -> (Object,a) -> (Object,[Term])
dataToTerms f (name,p) =
  let terms = mkTermsFromData p in
  let reducedTerms = S.toList $ S.fromList terms in
  (name, map show $ subsume $ catMaybes $ map f reducedTerms)

-- | Generically map immediate subterms recursively to discover all unique terms used
mkTermsFromData :: Data a => a -> [Term]
mkTermsFromData val =
  -- get type name
  let tn = show $ typeOf val in
  -- get constructor name (if algebraic), and generate list
  let ln = if isAlgType (dataTypeOf val) then [tn, show $ toConstr val] else [tn] in
  -- recursively evaluate sub-terms
  let subs = concat $ gmapQ (\d -> mkTermsFromData d) val in
  ln ++ subs

-- 3) Function that produces a formal context from program names & terms
mkFormalContext :: [(Object,[Attribute])] -> FormalContext
mkFormalContext ls =
  let g = map (\(obj,_)  -> obj) ls in
  let m = uniqueInSameOrder $ concatMap (\(_,atrs) -> atrs) ls in
  let i = uniqueInSameOrder $ concatMap (\(obj,atrs) -> map (\atr -> (obj,atr)) atrs) ls in
  (g, m, i)

-- | Object Closure
-- ' : A -> B, returns set of attributes which apply to all objects in A
-- corresponds to the object closure (objC)
objC :: [Object] -> FormalContext -> [Attribute]
objC [] _ = []
objC (x:ls) fc@(_,_,i) =
  let filt = filter (\(o,_) -> o == x) i in
  let cm = map snd filt in
  let rset = S.fromList (objC ls fc) in
  let atrs = if null rset then cm else S.toList $ (S.fromList cm) `S.intersection` rset in
  uniqueInSameOrder $ atrs

-- | Attribute Closure
-- ' : B -> A, returns set of objects which have all attributes in B
attrC :: [Attribute] -> FormalContext -> [Object]
attrC [] _ = []
attrC (x:ls) fc@(_,_,i) =
  let filt = filter (\(_,a) -> a == x) i in
  let cm = map fst filt in
  let rset = S.fromList (attrC ls fc) in
  let objs = if null rset then cm else S.toList $ (S.fromList cm) `S.intersection` rset in
  uniqueInSameOrder $ objs

-- find classifications through object analysis
-- Set A of objs can determine a classification
-- (A'',A')
-- A'' is the extent closure
mkClassificationsFromExtents :: FormalContext -> [Classification]
mkClassificationsFromExtents fc@(g,_,_) =
  map (\obj -> Classification (([obj],[],[]), attrC (objC [obj] fc) fc, objC [obj] fc)) g

-- find classifications through attribute analysis
-- or set B of attributes determines a classification
-- (B',B'')
-- B'' is the intent closure
mkClassificationsFromIntents :: FormalContext -> [Classification]
mkClassificationsFromIntents fc@(_,m,_) =
  map (\atr -> Classification (([],[atr],[]), attrC [atr] fc, objC (attrC [atr] fc) fc)) m

-- partially ordered by extents
-- (A1,B1) <= (A2,B2) iff A1 is a subset A2
compareClassificationsByExtents :: Classification -> Classification -> Maybe Ordering
compareClassificationsByExtents (Classification (_,a,_)) (Classification (_,b,_)) =
  let sa = S.fromList a
      sb = S.fromList b
  in
    case sa `S.isSubsetOf` sb of
      True  -> if sa == sb then Just EQ else Just LT
      False -> case sb `S.isSubsetOf` sa of
                True  -> Just GT
                False -> Nothing

-- | Builds a ProgramConcept Lattice from a list of classifications
mkProgramClassificationLattice :: [Classification] -> ClassificationLattice
mkProgramClassificationLattice xs = (xs, _mkProgramClassificationLattice xs xs)
  where
    -- internally constructs the relationships between classifications as edges
    _mkProgramClassificationLattice :: [Classification] -> [Classification] -> [(Classification,Classification)]
    _mkProgramClassificationLattice [] _  = []
    _mkProgramClassificationLattice ls bs =
      let x = head $ PO.maxima ls in -- get one of the maxima to work with
      -- compute direct lower neighbors
      let subClasses = filter (\y -> y PO.< x) bs in -- find all sub-classifications of our picked maxima
      -- get the most general sub-classifications that are direct neighbors of classification 'x' (i.e, the maxima)
      let neighbors = PO.maxima subClasses in
      -- creates edges from x -> neighbors
      let edges = map (\z -> (x,z)) neighbors in
      -- remove our plucked maxima from the running list
      let subList = ls \\ [x] in
      -- add edges, and continue
      edges ++ (_mkProgramClassificationLattice subList bs)
