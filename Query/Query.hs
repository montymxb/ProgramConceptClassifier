--
-- Represents a query performed on a concept graph
--

module Query (Query(Query),Path,query,queryGraph) where

import Concept
import ConceptGraph
import ConceptDependency
import Data.List (find,concat)
import Control.Monad (join)

-- represents a path of solutions
type Path a = [a]

{-
What does a query need?
- Known node (start point)
- Goal node (end point)

What does a query do?
- Represents a computation of 0 or more paths from start to end point, in a forward fashion

What does a query return?
- A list of possible paths, if any that go from start -> end
-}
data Query a =
  Query [a] a -- standard query goes [Path] -> End (Known -> Goal)
              -- known is expanded as the query is used to explore towards a goal node


-- construct a standard query with a single start point
query :: a -> a -> Query a
query start end = Query [start] end


-- generate multiple queries to a single point
queriesTo :: [a] -> a -> [Query a]
queriesTo [] _        = []
queriesTo (s:ls) (d)  = Query [s] d : queriesTo ls d

-- performs and combines multiple queries into one result
queryFromKnownToGoal :: (Eq a) => ConceptGraph b a -> [a] -> [a] ->  [Maybe (Path a)]
queryFromKnownToGoal _ _ [] = [Nothing]
queryFromKnownToGoal cg sl (d:dl) = join $ map (queryGraph cg) (queriesTo sl d)


-- produces a list of possible paths to the goal node
queryGraph :: (Eq a) => ConceptGraph b a -> Query a -> [Maybe (Path a)]
queryGraph _ (Query [] _) = [] -- nothing to query
queryGraph cg@(ConceptGraph concepts deps) q@(Query sls@(start:ls) end) | find (==(Concept start)) concepts == Nothing = [Nothing] -- node does not exist
                                                          | start == end = [Just sls] -- goal found add & stop here
                                                          | start /= end = join (map (queryGraph cg) (genNextQueries q deps))


-- generates a list of next possible queries to perform, if any
genNextQueries :: (Eq a) => Query a -> [ConceptDependency b a] -> [Query a]
genNextQueries _ [] = []
genNextQueries q@(Query sls@(start:sl) end) ((ConceptDependency _ c1 c2):ls) | c2 == start =((Query (c1:sls) end):(genNextQueries q ls)) -- gen new query and continue
                                                                  | c2 /= start = genNextQueries q ls -- skip to next dep

{- query algo outline
0. Verify start is in concepts, otherwise exit (not a viable solution)
1. Add start to path
2. If start already in path from prior computation, abort (done later)
3. If start is goal, return
3. Else, For each occurance of start as RHS of a dep
  4. Requery with the LHS as new start, and accumulate this path
5. Return Nothing, no path to add here
-}
