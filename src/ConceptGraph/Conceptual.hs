{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ConstrainedClassMethods #-}
--
--
--
module ConceptGraph.Conceptual where

import Data.Data
import ConceptGraph.Concept
import ConceptGraph.ConceptGraph
import ConceptGraph.ConceptDependency
import ConceptGraph.GraphToConceptGraph
import Data.List
import GVSpec.GVSpec as GVSpec
import Data.Maybe

-- | Removes duplicates from a list, whilst preserving order
_makeUnique :: (Eq a) => [a] -> [a]
_makeUnique [] = []
_makeUnique (x:ls) | elem x ls = _makeUnique ls -- drop, already present
                   | otherwise = x : _makeUnique ls-- keep it

-- | Removes duplicates in a list, but does not preserve order
makeUnique :: (Eq a) => [a] -> [a]
makeUnique = reverse . _makeUnique . reverse

-- | Gets all the edges that lead to concept 'n'
getAllEdgesFor :: (Eq a,Eq b) => a -> [(b,a,a)] -> [(b,a,a)]
getAllEdgesFor n ls = filter (\(_,from,_) -> from == n) ls

-- | Returns a list of edges that form cycles against an existing 'visited' list
identifyCycles :: (Eq a, Eq b) => [a] -> [(b,a,a)] -> [(b,a,a)]
identifyCycles visited edges = filter (\(_,_,x) -> elem x visited) edges

-- | Finds all cycles created by edges in this graph, looking for the farthest edge before a cycle is established from the root node
findCycles :: (Eq a, Eq b) => a -> [a] -> [(b,a,a)] -> [(b,a,a)]
findCycles _ _ [] = []
findCycles n visited ls = let edges = getAllEdgesFor n ls in           -- get all edges that start w/ 'n'
                                     let cycles = identifyCycles visited edges in -- foreach edge, if the 'to' element is in visited, it forms a cycle, skip it and return that cycle
                                     let goodEdges = edges \\ cycles in           -- get the good edges that do not form cycles
                                     let results = concatMap (\(_,_,b) -> findCycles b (n:visited) ls) goodEdges in -- reapply this check from all of those edges that do not form cycles
                                     cycles ++ results -- returns the results of this check and any other checks combined
                                     -- stops when there are no more edges to traverse (should end eventually)

-- | Removes cycles in a list of edges with regard to a given concept
removeCycles :: (Eq a, Eq b) => a -> [(b,a,a)] -> [(b,a,a)]
removeCycles _ [] = []
removeCycles n lsa = filter (\x -> not (elem x cycleEdges)) lsa where  -- only returns edges that do not form cycles
  cycleEdges = findCycles n [] lsa

-- | Removes 'list' around a type, to focus on the type itself
-- TODO this works ONLY in the context of BoGL, because Lists are not a type in the language
unbox :: String -> String
unbox [] = []
unbox ('[':x) = if reverse x !! 0 == ']' then init x else x
unbox x  = x

-- | Quotes a string
quote :: String -> String
quote s = '\'' : s ++ "'"

-- Verifies every edge ends leads to Concept
verifyLattice :: (Show b,Eq b) => [(b,String,String)] -> [(b,String,String)] -> (Bool,String)
verifyLattice [] _ = (True,"")
verifyLattice ((_,_,b):ls) ls2 | b /= "Concept" = let edges = getAllEdgesFor (unbox b) ls2 in
                               case edges of
                                 [] -> (False,"\n\nIn Lattice, " ++ quote b ++ " does not connect to any other node. It should be pointing to 'Concept' to complete the lattice.\n\nThis can be fixed by adding 'ebase s', where 's' is the element in question, and append this to the existing edge list.\n\n")
                                 _  -> verifyLattice ls ls2 -- continue
                                 | otherwise =  verifyLattice ls ls2-- skip to the next entry, if any

type EdgeName = String
type FromNode = String
type ToNode = String
type CNodes = [String]
type CEdges = [(EdgeName,FromNode,ToNode)]

-- dep type
data Dep = And | Or
  deriving Eq

instance Show Dep where
  show And = "orange"
  show Or = "blue"

-- manual adjustment that can be made to the graph
data GraphAdjustment = RemoveEdge (Dep,String,String) -- removes an edge
  | AddEdge (Dep,String,String) -- adds a new edge
  | RemoveNode String     -- removes an existing node
  | AddNode String        -- adds a new node
  | Rename String String  -- renames all nodes w/ edge updates that match From -> To
  | Pluck String          -- plucks a node, connecting the nodes that had edges going to/from this node

-- adjusts a graph
manuallyAdjustGraph :: [GraphAdjustment] -> ConceptGraph Dep String -> ConceptGraph Dep String
manuallyAdjustGraph [] cg = cg
manuallyAdjustGraph (a:ls) cg = manuallyAdjustGraph ls (applyAdjustment cg a)

makeNewEdges :: [(Dep,String)] -> [(Dep,String)] -> [(Dep,String,String)]
makeNewEdges [] _ = []
makeNewEdges ((d1,x):ls) outgoing = (map (\(d2,z) -> (if d1 == And then d1 else d2,x,z)) outgoing) ++ makeNewEdges ls outgoing

applyAdjustment :: ConceptGraph Dep String -> GraphAdjustment -> ConceptGraph Dep String
applyAdjustment (ConceptGraph nodes deps) (RemoveEdge edge) = ConceptGraph nodes (filter (\x -> x /= edge) deps)
applyAdjustment (ConceptGraph nodes deps) (AddEdge edge)    = ConceptGraph nodes (edge:deps)
applyAdjustment (ConceptGraph nodes deps) (RemoveNode node) = ConceptGraph (filter (\(Concept x) -> x /= node) nodes) deps
applyAdjustment (ConceptGraph nodes deps) (AddNode node)    = ConceptGraph ((Concept node):nodes) deps
applyAdjustment (ConceptGraph nodes deps) (Rename from to)  = ConceptGraph
                                                                (map (\(Concept x) -> if x == from then (Concept to) else (Concept x)) nodes)
                                                                (map (\(d,c1,c2) -> (d, if c1 == from then to else c1, if c2 == from then to else c2)) deps)
applyAdjustment (ConceptGraph nodes deps) (Pluck n)          = let newNodes = (filter (\(Concept x) -> x /= n) nodes) in                        -- remove this  node
                                                               let ingoing = map (\(d,c1,_) -> (d,c1)) (filter (\(_,_,c2) -> c2 == n) deps) in  -- find all incoming edges
                                                               let outgoing = map (\(d,_,c2) -> (d,c2)) (filter (\(_,c1,_) -> c1 == n) deps) in -- get all outgoing edges
                                                               let newDeps = deps ++ makeNewEdges ingoing outgoing in                           -- add new deps
                                                               let withoutOrig = (filter (\(d,x,y) -> not(x == n || y == n)) newDeps) in             -- remove the original edges from the new deps
                                                               ConceptGraph newNodes withoutOrig

-- replace tuple & cons instances
cleanupConcept :: String -> String
cleanupConcept "(,)" = "Tuple"
cleanupConcept "(:)" = "Cons"
cleanupConcept n = n

-- ground a possible leaf to 'Concept', if necessary
groundConcept :: [(Dep,String,String)] -> String -> Maybe (Dep,String,String)
groundConcept [] s = Just (And,s,"Concept")
groundConcept ((_,c1,c2):ls) s | s == c1 && s /= c2 = Nothing
                               | otherwise = groundConcept ls s

correctCons :: [(Dep,String,String)] -> [(Dep,String,String)]
correctCons [] = []
correctCons (i@(d,c1,c2):ls) | c1 == "Cons" && c2 !! 0 /= '[' = (d,"[" ++ c2 ++ "]",c2) : (correctCons ls) -- ammend it
                             | c1 == "Cons" || c2 == "Cons" = correctCons ls -- drop it
                             | otherwise = i : (correctCons ls)

-- produce graph dependencies from Data type
getGraphDependencies :: Data a => a -> [(Dep,String,String)]
getGraphDependencies val =
                let tn = show $ typeOf val in -- 1. get type name
                -- 2. get constructor name
                let cn = if isAlgType (dataTypeOf val) then show $ toConstr val else tn in
                let cn2 = if cn == "[]" then "List" else cn in
                -- 3. same for subterms
                let immediateAnds = gmapQ (\d -> (And,cn2,show $ typeOf d)) val in
                -- run this for sub-terms as well
                let subs = concat $ gmapQ (\d -> getGraphDependencies d) val in
                (Or,tn,cn2) : (subs ++ immediateAnds)

-- builds a concept graph from a value of a data type
conceptGraph :: Data a => a -> ConceptGraph Dep String--([String],[(Dep,String,String)])
conceptGraph val = let edges = getGraphDependencies val in
                     let concepts = filter (\x -> x /= "Cons") $ "Concept":map cleanupConcept (uniqueConcepts edges) in
                     let ue = map (\(d,c1,c2) -> (d,cleanupConcept c1,cleanupConcept c2)) (makeUnique edges) in
                     let uniqueEdges = correctCons ue in
                     -- remove self edges
                     -- and ground all concepts
                     -- remove all edges started by list,
                     -- add an edge from List -> Concept
                     let updatedUniqueEdges = (filter (\(_,c1,c2) -> c1 /= c2) $ uniqueEdges ++ catMaybes (map (groundConcept uniqueEdges) concepts)) in
                     --let reducedEdges = foldl (\tot x -> removeCycles x tot) updatedUniqueEdges concepts in
                     --let verifyResult = verifyLattice updatedUniqueEdges updatedUniqueEdges in
                     graph_to_concept_graph (concepts,updatedUniqueEdges)
                     {-
                     case verifyResult of
                       (True,_)     -> graph_to_concept_graph (concepts,reducedEdges)
                       (False,msg)  -> error ("Unable to verify lattice with error: " ++ msg)
                     -}
                     where
                       uniqueConcepts ls = makeUnique $ concatMap (\(_,c1,c2) -> [c1,c2]) ls

--
-- Order Checking
--
type Known a = [Concept a]

-- takes a concept, list of deps, and produces concepts we are dependent on, and those dependent concepts and so forth
deps :: (Data a, Eq a) => Concept a -> Known a -> [ConceptDependency b a] -> [Concept a]
deps _ _ []     = []
deps c@(Concept c1) k ((_,frm,to):ls) | c1 == frm && not (elem (Concept to) k) = ((Concept to) : deps (Concept to) k ls) ++ deps c k ls -- only add deps that we don't already know
                                    | otherwise = deps c k ls

-- represents an order check result
data KnownCheck = OK
  | Unknown String
  deriving (Show)

-- Extracts the underlying value from a concept node
unconcept :: Concept a -> a
unconcept (Concept a) = a

-- | Returns a KnownCheck result for a given list of known concepts and a concept graph
isKnown :: Known String -> ConceptGraph b String -> KnownCheck
isKnown _ (ConceptGraph [] _)        = OK  -- nothing is required to understand nothing
isKnown [] (ConceptGraph _ _)        = Unknown "If nothing is known, then nothing can be known besides nothing (empty set of knowledge equates empty set of understanding)"
isKnown k (ConceptGraph (a:ls) cds)  =  case elem a k of
  -- this is a pre-known concept, skip it, even if we don't know some of it's deps
  True -> isKnown (a:k) (ConceptGraph ls cds)
  -- this is not a previously known concept, scrutinize it to determine if we can figure it out
  False ->  let d = makeUnique (deps a k cds) in -- get deps
            let knownChecks = map (\x -> elem x k) d in -- check which deps we know
            let allKnown = all (\x -> x) knownChecks in -- verify we know all the deps
            case allKnown of
              True   -> isKnown (a:k) (ConceptGraph ls cds) -- check the next concept, adding 'a' to the known list
              False  -> Unknown $ "Concept " ++ (quote . unconcept) a ++ " was determined to be unknown with regards to dependencies:\n" ++ intercalate "\n" (map unconcept ((filter (\x -> not(elem x k)) d)))

-- | Produce graphs from programs
produceGraphs :: (Data a, Eq a) => [a] -> [ConceptGraph Dep String]
produceGraphs = map conceptGraph

-- write it as a reduce
_graphSimpleProgs :: [ConceptGraph Dep String] -> Int -> IO ()
_graphSimpleProgs [] _ = return ()
_graphSimpleProgs (x:ls) i = do
  GVSpec.writeGVSpec ("simple" ++ (show i)) x
  _graphSimpleProgs ls (i+1)

-- write it as a reduce
_graphSimpleProgsWithName :: [ConceptGraph EdgeName String] -> Int -> String -> IO ()
_graphSimpleProgsWithName [] _ _ = return ()
_graphSimpleProgsWithName (x:ls) i n = do
  GVSpec.writeGVSpec (n ++ (show i)) x
  _graphSimpleProgsWithName ls (i+1) n

-- Graphs a concept graph
graphConceptGraph :: ConceptGraph Dep String -> String -> IO ()
graphConceptGraph cg name = GVSpec.writeGVSpec name cg

-- | Shows the result of whether a program is known
showRez :: KnownCheck -> IO ()
showRez (Unknown s) = putStrLn $ "\n\n" ++ s ++ "\n\n"
showRez OK = putStrLn "\n\nAll concepts known and OK\n\n"

-- | Extract knowns
_knowns :: Int -> [Concept String] -> [ConceptGraph Dep String] -> IO ()
_knowns _ _ [] = return ()
_knowns i kl (x:ls) = do
  putStrLn ("For Program " ++ (show i))
  showRez (isKnown kl x)
  _knowns (i+1) kl ls

-- | Produces the concept diffs across a list of programs, and in the order introduced
produceDiffs :: [Concept String] -> [ConceptGraph Dep String] -> [[Concept String]]
produceDiffs _ [] = []
produceDiffs knowns ((ConceptGraph concepts _):ls) = [concepts \\ knowns] ++ (produceDiffs (_makeUnique (knowns ++ concepts)) ls)

-- Calculates the std deviation of the size of the unique sets of concepts introduced in each example
stdDeviation :: [[a]] -> Float
stdDeviation ls = let len = fromIntegral (length ls) in
                  let s1 = (foldl (\ss x -> (fromIntegral (length x)) + ss) (0.0 :: Float) ls) in
                  let xmean = s1 / len in
                  let summ = foldl (\ss x -> ss + ((fromIntegral (length x)) - xmean) ^ 2) (0.0 :: Float) ls in
                  let result = sqrt (summ / (len-1)) in
                  result
