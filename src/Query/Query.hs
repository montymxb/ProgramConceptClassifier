{-
Query.hs

Represents queries that can be performed on a concept graph

Outright Kinds of Queries

queryDeps - Returns immediate dependencies that exist for a concept
queryDepsRecur - Returns all dependencies that exist for a concept, and it's dependent concepts
queryUnknown - Given a list of concepts that are known, and a graph of unknown concepts, attempt to resolve all concepts, and return those that are not understood
queryToGoal - From a concept graph, and a known set, query to the goal
queryKnown - using a concept graph, extracts a list of known concepts, assuming

Query Select Constraints

- FIRST, ALL - positional modifiers, either the first or all
- containing [...] - list of concepts that a query result must contain
- notContaining [...] - list of concepts that a query result must not contain

Query Ordering Constraints (orderBy)

- none, perform no ordering on the examples
- ascendingComplexity [...] - return in order of ascending result complexity
- descendingComplexity [...] - return in order of desecnding complexity

Query

run :: Query -> Result

query :: ConceptGraph -> Query -> Result

Result can be either a list of Concepts (aggregation) or a list of paths (solution), where  a path is a graph in itself

Programs are distilled down into concepts
Program -> Concepts
A knowledge base is composed of a ([Known],[Goal],GroundTruth), such that we can know where we are at all times
Semantic domain of queries, produces list of programs, so a query is a list o fprograms

-}

module Query.Query where

import ConceptGraph.Concept
import ConceptGraph.ConceptGraph
import ConceptGraph.ConceptDependency
import Data.List (find)
import Control.Monad (join)
import Data.Set (toList,fromList)
import Data.Maybe(catMaybes)
import DB.BoglDB
import Parser.Parser

data Constraint =
  First |
  All |
  Containing String |
  NotContaining String |
  AscendingComplexity |
  DescendingComplexity

-- queries are performed by selecting first or All from
-- a known to a goal program, with optional does/doesn't contains and a possible ordering at the end
-- data Query a = Query Select (Known a) (Goal a) [Containing a] OrderBy


-- | query result is a list of items
-- type QueryResult a = [a]


-- | Defines what a query for a concept graph is. A query returns a concept lattice, which is a sub-set of the concept graph
--data Query a =
--  Query [a] a -- ^ standard query goes [Path] -> End (Known -> Goal)
              -- known is expanded as the query is used to explore towards a goal node


-- | Constructs a simple query with a single start point
--query :: a -> a -> Query a
--query start end = Query [start] end
type Program = String
type Known = Program
type Goal = Program

-- convert BoGL program, into our concept graph...involves mapping things by hand...time consuming
-- TODO map from bogl syntax into concept graph
{- TODO BRING THIS BACK
conceptualize :: [IO (Either ParseError (Game SourcePos))] -> [IO (ConceptGraph String String)]
conceptualize ls = undefined

produceCGFromBoGLProg :: String -> ConceptGraph String String
produceCGFromBoGLProg s = let parsedGame = parseGameFile s in
                          conceptualize parsedGame

loadExampleProgs :: [(Program,ConceptGraph String String)]
loadExampleProgs = undefined --getDB "examples" in

solveForConstraints :: [String] -> [String] -> [(Program,ConceptGraph String String)] -> [Constraint] -> [(Program,ConceptGraph String String)]
solveForConstraints known goal progs constraints = undefined
-}

-- TODO, refinement may be used for post filtering (such as 1st or w/e)
--refine :: [Constraint] -> [(Program,ConceptGraph String String)] -> [(Program,ConceptGraph String String)]
--refine [] ls     = ls
--refine (_:cl) ls = undefined --refine (:cl) ls = ls

-- query from a Known to a Goal Program, w/ given constraints
{- TODO BRING THIS BACK
query :: Known -> Goal -> [Constraint] -> [[(Program,ConceptGraph String String)]] -- TODO something is bugged out here...
                                       -- produce concept graph from known prog
query knownProg goalProg constraints = do
                                     let cgKnown = produceCGFromBoGLProg knownProg
                                     -- produce concept graph from goal prog
                                     let cgGoal = produceCGFromBoGLProg goalProg
                                     -- get list of known concepts from cgKnown (known concept graph)
                                     let knownConcepts = concepts cgKnown
                                     -- get list of goal concepts from cgGoal (goal concept graph)
                                     let goalConcepts = concepts cgGoal
                                     -- load up list of processed programs :: (ProgName,ConceptGraph)
                                     -- needs a new function
                                     let exProgs = loadExampleProgs
                                     -- retrieve a list of programs that complete the path from known -> goal
                                     let paths = solveForConstraints knownConcepts goalConcepts exProgs constraints
                                     -- constrain the path results further
                                     --let constrainedPaths = refine constraints paths
                                     -- return the constrained paths
                                     return paths
                                     -}



-- | Produces a list of concept dependencies from a list of paths
{-
getEdgesFromPaths :: b -> [a] -> [ConceptDependency b a]
getEdgesFromPaths _ []      = [] -- no items
getEdgesFromPaths _ (_:[]) = [] -- last item
getEdgesFromPaths e (p1:(p2:ls)) = ((e,p1,p2) : (getEdgesFromPaths e (p2:ls))) -- dep to add


-- | Gets the unique vertices present across all paths
getUniqueVertices :: Ord a => [Path a] -> [Concept a]
getUniqueVertices xl = map (\x -> Concept x) (toList $ fromList $ (concat xl))
-}


-- | Filters paths for a given concept graph into a concept lattice, using the same concept depencies for all edges
{-
_filterPathsIntoLattice :: (Eq a, Ord a, Ord b) => (ConceptGraph b a) -> [Maybe (Path a)] -> ConceptLattice b a
_filterPathsIntoLattice (ConceptGraph _ ((e1,_,_):_)) mpaths = let paths = catMaybes mpaths in
                  let edges   = toList $ fromList $ concat $ map (getEdgesFromPaths e1) paths in
                  let vertices= getUniqueVertices paths in
                  (ConceptGraph vertices edges)
_filterPathsIntoLattice _ _ = undefined
-}



--
--
-- Multi-Query from [Known] -> [Goal]
--
--

{-
-- | Performs multiple queries from various src/goal pairs, and unifies the final result into a concept graph
queryFromKnownToGoal :: (Eq a, Ord a, Ord b) => (ConceptGraph b a) -> [a] -> [a] -> ConceptGraph b a
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
-}



--
--
-- Chained Querying, A -> B -> C
--
--


-- | Generates a chain of queries from concept to concept
{-
genInOrderQueries :: (Eq a) => [a] -> [Query a]
genInOrderQueries []            = []
genInOrderQueries (_:[])        = []
genInOrderQueries (i1:(i2:ls))  = (Query [i1] i2) : (genInOrderQueries (i2:ls))


-- | Perform queries in order, from one node to another node, until done
queryInOrder :: (Eq a, Ord a, Ord b) => ConceptGraph b a -> [a] -> ConceptLattice b a
queryInOrder cg concepts = _filterPathsIntoLattice cg (concat (map (_queryGraph cg) (genInOrderQueries concepts)))
-}



--
--
-- Standard Querying
--
--
{-
-- | Queries a concept graph and returns a concept lattice
queryGraph :: (Eq a, Ord a, Ord b) => ConceptGraph b a -> Query a -> ConceptLattice b a
queryGraph cg q = _filterPathsIntoLattice cg (_queryGraph cg q)


-- | Produces a list of possible paths to a goal concept from a given concept
_queryGraph :: (Eq a) => ConceptGraph b a -> Query a -> [Maybe (Path a)]
_queryGraph _ (Query [] _) = [] -- nothing to query
_queryGraph cg@(ConceptGraph concepts deps) q@(Query sls@(start:_) end) | find (==(Concept start)) concepts == Nothing = [Nothing] -- node does not exist
                                                          | start == end = [Just sls] -- goal found, add & stop here
                                                          | start /= end = join (map (_queryGraph cg) (genNextQueries q deps)) -- continue querying from here
_queryGraph _ _ = undefined -- TODO, unhandled


-- | Generates a list of next possible queries to perform, if any
genNextQueries :: (Eq a) => Query a -> [ConceptDependency b a] -> [Query a]
genNextQueries _ [] = []
genNextQueries q@(Query sls@(start:_) end) ((_,c1,c2):ls) | c2 == start =((Query (c1:sls) end):(genNextQueries q ls)) -- gen new query and continue
                                                                  | c2 /= start = genNextQueries q ls -- skip to next dep
genNextQueries _ _ = undefined
-}
