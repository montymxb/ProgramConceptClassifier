--
-- Represents a query performed on a concept graph
--

module Query (Query(Query),Path,query,queryGraph,queryFromKnownToGoal,_queryFromKnownToGoal,_queryGraph) where

import Concept
import ConceptGraph
import ConceptDependency
import Data.List (find,concat)
import Control.Monad (join)
import Data.Set (toList,fromList)
import Data.Maybe(catMaybes)
import GrammarToGraph

-- | Path of solutions
type Path a = [a]


-- | Defines what a query for a concept graph is. A query returns a concept lattice, which is a sub-set of the concept graph
data Query a =
  Query [a] a -- ^ standard query goes [Path] -> End (Known -> Goal)
              -- known is expanded as the query is used to explore towards a goal node


-- | Constructs a simple query with a single start point
query :: a -> a -> Query a
query start end = Query [start] end


-- | Generates multiple queries from different source concepts to a single destination concept
queriesTo :: [a] -> a -> [Query a]
queriesTo [] _        = []
queriesTo (s:ls) (d)  = Query [s] d : queriesTo ls d

-- | Filters paths for a given concept graph into a concept lattice, using the same concept depencies for all edges
_filterPathsIntoLattice :: (Eq a, Ord a, Ord b) => (ConceptGraph b a) -> [Maybe (Path a)] -> ConceptLattice b a
_filterPathsIntoLattice cg@(ConceptGraph _ ((ConceptDependency e1 _ _):_)) mpaths = let paths = catMaybes mpaths in
                  let edges   = toList $ fromList $ concat $ map (getEdgesFromPaths e1) paths in
                  let vertices= getUniqueVertices paths in
                  (ConceptGraph vertices edges)

-- | Performs multiple queries from various src/goal pairs, and unifies the final result into a concept graph (not a lattice in this case)
queryFromKnownToGoal :: (Eq a, Ord a, Ord b) => (ConceptGraph b a) -> [a] -> [a] -> ConceptLattice b a
queryFromKnownToGoal cg src dst = _filterPathsIntoLattice cg (_queryFromKnownToGoal cg src dst)

-- | Evaluates and combines the results of multiple queries into a single list of possible paths
-- Takes a concept graph, a list of start concepts, and a list of destination concepts
_queryFromKnownToGoal :: (Eq a, Ord a) => (ConceptGraph b a) -> [a] -> [a] ->  [Maybe (Path a)]
_queryFromKnownToGoal _ _ [] = [Nothing]
_queryFromKnownToGoal cg sl (d:dl) = join $ map (_queryGraph cg) (queriesTo sl d)

-- | From a given list of paths, produces a list of concept dependencies
getEdgesFromPaths :: b -> [a] -> [ConceptDependency b a]
getEdgesFromPaths _ []      = [] -- no items
getEdgesFromPaths _ (p1:[]) = [] -- last item
getEdgesFromPaths e (p1:(p2:ls)) = ((ConceptDependency e p1 p2) : (getEdgesFromPaths e (p2:ls))) -- dep to add

-- | Gets the unique vertices present across all paths
getUniqueVertices :: Ord a => [Path a] -> [Concept a]
getUniqueVertices xl = map (\x -> Concept x) (toList $ fromList $ (concat xl))


-- | Queries a concept graph with a query, and returns a concept lattice
queryGraph :: (Eq a, Ord a, Ord b) => ConceptGraph b a -> Query a -> ConceptLattice b a
queryGraph cg q = _filterPathsIntoLattice cg (_queryGraph cg q)

-- | Produces a list of possible paths to the goal concept
_queryGraph :: (Eq a) => ConceptGraph b a -> Query a -> [Maybe (Path a)]
_queryGraph _ (Query [] _) = [] -- nothing to query
_queryGraph cg@(ConceptGraph concepts deps) q@(Query sls@(start:ls) end) | find (==(Concept start)) concepts == Nothing = [Nothing] -- node does not exist
                                                          | start == end = [Just sls] -- goal found, add & stop here
                                                          | start /= end = join (map (_queryGraph cg) (genNextQueries q deps)) -- continue querying from here


-- | Generates a list of next possible queries to perform, if any
genNextQueries :: (Eq a) => Query a -> [ConceptDependency b a] -> [Query a]
genNextQueries _ [] = []
genNextQueries q@(Query sls@(start:sl) end) ((ConceptDependency _ c1 c2):ls) | c2 == start =((Query (c1:sls) end):(genNextQueries q ls)) -- gen new query and continue
                                                                  | c2 /= start = genNextQueries q ls -- skip to next dep
