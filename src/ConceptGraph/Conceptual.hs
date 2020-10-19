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

-- Shorthand for showing the constructor used to build value 'x'
toConstr' :: Data a => a -> String
toConstr' x = show (toConstr x)

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
removeCycles n lsa@(s@(_,a,c):ls) = filter (\x -> not (elem x cycleEdges)) lsa where  -- only returns edges that do not form cycles
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
verifyLattice (x@(_,a,b):ls) ls2 | b /= "Concept" = let edges = getAllEdgesFor (unbox b) ls2 in
                               case edges of
                                 [] -> (False,"\n\nIn Lattice, " ++ quote b ++ " does not connect to any other node. It should be pointing to 'Concept' to complete the lattice.\n\nThis can be fixed by adding 'ebase s', where 's' is the element in question, and append this to the existing edge list.\n\n")
                                 els-> verifyLattice ls ls2 -- continue
                                 | otherwise =  verifyLattice ls ls2-- skip to the next entry, if any

type EdgeName = String
type FromNode = String
type ToNode = String
type CNodes = [String]
type CEdges = [(EdgeName,FromNode,ToNode)]
class Conceptual a where
  cgraph :: (Typeable a, Data a, Eq a) => a -> (CNodes,CEdges)
  cgraph x = let uniqueConcepts = (makeUnique . concepts) x in
             let uniqueEdges = (makeUnique . _edges) x in
             let reducedEdges = foldl (\tot x -> removeCycles x tot) uniqueEdges uniqueConcepts in
             let verifyResult = verifyLattice reducedEdges reducedEdges in
             case (verifyResult) of
               (True,_)    -> (uniqueConcepts, reducedEdges)
               (False,msg) -> error ("Unable to verify lattice with error: " ++ msg)

  concepts :: (Typeable a, Data a, Eq a) => a -> CNodes
  concepts c = [show (typeOf c), toConstr' c, "Concept"]

  _edges :: (Typeable a, Data a, Eq a) => a -> CEdges
  -- tie 'a' from type -> constr name -> Concept, assuming this is a base type
  _edges x = [("",show (typeOf x), toConstr' x),("",toConstr' x, "Concept")]

  -- Builds an edge from X -> Y
  getEdges :: (Data a, Typeable a) => String -> a -> [(EdgeName,FromNode,ToNode)]
  getEdges cname a = [("",cname,show (typeOf a))]



instance (Data a, Eq a, Conceptual a) => Conceptual [a] where
  concepts x = concatMap concepts x

  _edges []   = []
  _edges ls   = concatMap _edges ls

  getEdges cname [] = []
  getEdges cname ls = map (\y -> ("",cname,show (typeOf y))) (ls)


-- Self edge
selfEdge :: (Data a, Typeable a) => a -> (EdgeName,FromNode,ToNode)
selfEdge x = ("",show (typeOf x), toConstr' x)

instance Conceptual Int where
  concepts _ = ["Int","Concept"]
  _edges _ = [("","Int","Concept")]

instance Conceptual Double where
  concepts _ = ["Double","Concept"]
  _edges _ = [("", "Double", "Concept")]

instance Conceptual Float where
  concepts _ = ["Float","Concept"]
  _edges _ = [("", "Float", "Concept")]

instance Conceptual Word where
  concepts _ = ["Word","Concept"]
  _edges _ = [("", "Word", "Concept")]

instance Conceptual Bool where
  concepts _ = ["Bool","Concept"]
  _edges _ = [("", "Bool", "Concept")]

instance Conceptual Char where
  concepts _ = ["Char","Concept"]
  _edges _ = [("", "Char", "Concept")]

ccs :: (Typeable a, Data a) => a -> CNodes
ccs s = [(show (typeOf s)),toConstr' s]

--
-- The following functions are defined such that no data constructor in the target language has more parameters than any of the following
-- Allows us to more generally parse the language.
--

c0 :: (Data a, Typeable a) => a -> CNodes
c0 a = ccs a

-- base concept
cbase :: (Data a, Typeable a) => a -> CNodes
cbase a = [toConstr' a, "Concept"]

cb :: (Data b, Typeable b, Conceptual b, Eq b) => b -> CNodes
cb = concepts

c1 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b) => a -> b -> CNodes
c1 a b = c0 a ++ cb b

c2 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c) => a -> b -> c -> CNodes
c2 a b c = c0 a ++ cb b ++ cb c

c3 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d) => a -> b -> c -> d -> CNodes
c3 a b c d = c0 a ++ cb b ++ cb c ++ cb d

c4 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d, Eq e, Data e, Typeable e, Conceptual e) => a -> b -> c -> d -> e -> CNodes
c4 a b c d e = c0 a ++ cb b ++ cb c ++ cb d ++ cb e

c5 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d, Eq e, Data e, Typeable e, Conceptual e, Eq f, Data f, Typeable f, Conceptual f) => a -> b -> c -> d -> e -> f -> CNodes
c5 a b c d e f = c0 a ++ cb b ++ cb c ++ cb d ++ cb e ++ cb f

--
-- The following functions are defined such that no data constructor in the target language has more parameters than any of the following
-- Allows us to more generally parse the language.
--

-- basing edge
ebase :: (Data a, Typeable a) => a -> CEdges
ebase a = [("",show (typeOf a),toConstr' a),("",toConstr' a,"Concept")]

e0 :: (Data a, Typeable a) => a -> CEdges
e0 a = [selfEdge a]

e1 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b) => a -> b -> CEdges
e1 a b = [selfEdge a] ++ (getEdges cname b) ++ _edges b where
  cname = toConstr' a

e2 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c) => a -> b -> c -> CEdges
e2 a b c = [selfEdge a] ++ (getEdges cname b) ++ (getEdges cname c) ++ _edges b ++ _edges c where
  cname = toConstr' a

e3 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d) => a -> b -> c -> d -> CEdges
e3 a b c d = [selfEdge a] ++ (getEdges cname b) ++ (getEdges cname c) ++ (getEdges cname d) ++ _edges b ++ _edges c ++ _edges d where
  cname = toConstr' a

e4 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d, Eq e, Data e, Typeable e, Conceptual e) => a -> b -> c -> d -> e -> CEdges
e4 a b c d e = [selfEdge a] ++ (getEdges cname b) ++ (getEdges cname c) ++ (getEdges cname d) ++ (getEdges cname e) ++ _edges b ++ _edges c ++ _edges d ++ _edges e where
  cname = toConstr' a

e5 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d, Eq e, Data e, Typeable e, Conceptual e, Eq f, Data f, Typeable f, Conceptual f) => a -> b -> c -> d -> e -> f -> CEdges
e5 a b c d e f = [selfEdge a] ++ (getEdges cname b) ++ (getEdges cname c) ++ (getEdges cname d) ++ (getEdges cname e) ++ (getEdges cname f) ++ _edges b ++ _edges c ++ _edges d ++ _edges e ++ _edges f where
  cname = toConstr' a

--
-- Order Checking
--
type Known a = [Concept a]

-- takes a concept, list of deps, and produces concepts we are dependent on, and those dependent concepts and so forth
deps :: (Conceptual a, Eq a) => Concept a -> Known a -> [ConceptDependency b a] -> [Concept a]
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
produceGraphs :: (Conceptual a, Data a, Eq a) => [a] -> [ConceptGraph EdgeName String]
produceGraphs = map (graph_to_concept_graph . cgraph)

-- write it as a reduce
_graphSimpleProgs :: [ConceptGraph EdgeName String] -> Int -> IO ()
_graphSimpleProgs [] _ = return ()
_graphSimpleProgs (x:ls) i = do
  GVSpec.writeGVSpec ("simple" ++ (show i)) x
  _graphSimpleProgs ls (i+1)

-- | Shows the result of whether a program is known
showRez :: KnownCheck -> IO ()
showRez (Unknown s) = putStrLn $ "\n\n" ++ s ++ "\n\n"
showRez OK = putStrLn "\n\nAll concepts known and OK\n\n"

-- | Extract knowns
_knowns :: Int -> [Concept String] -> [ConceptGraph EdgeName String] -> IO ()
_knowns _ _ [] = return ()
_knowns i kl (x:ls) = do
  putStrLn ("For Program " ++ (show i))
  showRez (isKnown kl x)
  _knowns (i+1) kl ls

-- | Produces the concept diffs across a list of programs, and in the order introduced
produceDiffs :: [Concept String] -> [ConceptGraph EdgeName String] -> [[Concept String]]
produceDiffs _ [] = []
produceDiffs knowns ((ConceptGraph concepts deps):ls) = [concepts \\ knowns] ++ (produceDiffs (_makeUnique (knowns ++ concepts)) ls)
