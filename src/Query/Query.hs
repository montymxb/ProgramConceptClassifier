--
-- Represents a query performed on a concept graph
--

module Query.Query (Query(Query),Path,query,queryGraph,queryFromKnownToGoal,_queryFromKnownToGoal,_queryGraph,queryInOrder) where

import ConceptGraph.Concept
import ConceptGraph.ConceptGraph
import ConceptGraph.ConceptDependency
import Grammar.GrammarToGraph

import Data.List (find,concat)
import Control.Monad (join)
import Data.Set (toList,fromList)
import Data.Maybe(catMaybes)


-- | Path of solutions
type Path a = [a]


-- | Defines what a query for a concept graph is. A query returns a concept lattice, which is a sub-set of the concept graph
data Query a =
  Query [a] a -- ^ standard query goes [Path] -> End (Known -> Goal)
              -- known is expanded as the query is used to explore towards a goal node



-- | Constructs a simple query with a single start point
query :: a -> a -> Query a
query start end = Query [start] end


-- | Produces a list of concept dependencies froma list of paths
getEdgesFromPaths :: b -> [a] -> [ConceptDependency b a]
getEdgesFromPaths _ []      = [] -- no items
getEdgesFromPaths _ (p1:[]) = [] -- last item
getEdgesFromPaths e (p1:(p2:ls)) = ((e,p1,p2) : (getEdgesFromPaths e (p2:ls))) -- dep to add


-- | Gets the unique vertices present across all paths
getUniqueVertices :: Ord a => [Path a] -> [Concept a]
getUniqueVertices xl = map (\x -> Concept x) (toList $ fromList $ (concat xl))


-- | Filters paths for a given concept graph into a concept lattice, using the same concept depencies for all edges
_filterPathsIntoLattice :: (Eq a, Ord a, Ord b) => (ConceptGraph b a) -> [Maybe (Path a)] -> ConceptLattice b a
_filterPathsIntoLattice cg@(ConceptGraph _ ((e1,_,_):_)) mpaths = let paths = catMaybes mpaths in
                  let edges   = toList $ fromList $ concat $ map (getEdgesFromPaths e1) paths in
                  let vertices= getUniqueVertices paths in
                  (ConceptGraph vertices edges)



--
--
-- Multi-Query from [Known] -> [Goal]
--
--

-- | Performs multiple queries from various src/goal pairs, and unifies the final result into a concept graph (not always a lattice in this case)
queryFromKnownToGoal :: (Eq a, Ord a, Ord b) => (ConceptGraph b a) -> [a] -> [a] -> ConceptLattice b a
queryFromKnownToGoal cg src dst = _filterPathsIntoLattice cg (_queryFromKnownToGoal cg src dst)


-- | Generates multiple queries from different source concepts to a single destination concept
queriesTo :: [a] -> a -> [Query a]
queriesTo [] _        = []
queriesTo (s:ls) (d)  = Query [s] d : queriesTo ls d


-- | Evaluates and combines the results of multiple queries into a single list of possible paths
-- Takes a concept graph, a list of start concepts, and a list of destination concepts
_queryFromKnownToGoal :: (Eq a, Ord a) => (ConceptGraph b a) -> [a] -> [a] ->  [Maybe (Path a)]
_queryFromKnownToGoal _ _ [] = [Nothing]
_queryFromKnownToGoal cg sl (d:dl) = (join $ map (_queryGraph cg) (queriesTo sl d)) ++ (_queryFromKnownToGoal cg sl dl)




--
--
-- Chained Querying, A -> B -> C
--
--


-- | Generates a chain of queries from concept to concept
genInOrderQueries :: (Eq a) => [a] -> [Query a]
genInOrderQueries []            = []
genInOrderQueries (_:[])        = []
genInOrderQueries (i1:(i2:ls))  = (Query [i1] i2) : (genInOrderQueries (i2:ls))


-- | Perform queries in order, from one node to another node, until done
queryInOrder :: (Eq a, Ord a, Ord b) => ConceptGraph b a -> [a] -> ConceptLattice b a
queryInOrder cg concepts = _filterPathsIntoLattice cg (concat (map (_queryGraph cg) (genInOrderQueries concepts)))




--
--
-- Standard Querying
--
--

-- | Queries a concept graph and returns a concept lattice
queryGraph :: (Eq a, Ord a, Ord b) => ConceptGraph b a -> Query a -> ConceptLattice b a
queryGraph cg q = _filterPathsIntoLattice cg (_queryGraph cg q)


-- | Produces a list of possible paths to a goal concept from a given concept
_queryGraph :: (Eq a) => ConceptGraph b a -> Query a -> [Maybe (Path a)]
_queryGraph _ (Query [] _) = [] -- nothing to query
_queryGraph cg@(ConceptGraph concepts deps) q@(Query sls@(start:ls) end) | find (==(Concept start)) concepts == Nothing = [Nothing] -- node does not exist
                                                          | start == end = [Just sls] -- goal found, add & stop here
                                                          | start /= end = join (map (_queryGraph cg) (genNextQueries q deps)) -- continue querying from here


-- | Generates a list of next possible queries to perform, if any
genNextQueries :: (Eq a) => Query a -> [ConceptDependency b a] -> [Query a]
genNextQueries _ [] = []
genNextQueries q@(Query sls@(start:sl) end) ((_,c1,c2):ls) | c2 == start =((Query (c1:sls) end):(genNextQueries q ls)) -- gen new query and continue
                                                                  | c2 /= start = genNextQueries q ls -- skip to next dep
