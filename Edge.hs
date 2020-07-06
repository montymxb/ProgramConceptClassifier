--
-- Edge.hs
--
--

module Edge where

import Rule
import Node

import Data.List


data EdgeCategory =
  LangDep  -- language dependency
  deriving (Show)

-- represents a relationship between symbols
type Edge = (Node,Node,EdgeCategory)


-- get an edge from a pair of symbols
get_edge_from_symbols :: Node -> Node -> Edge
get_edge_from_symbols s1 s2 = (s1,s2,LangDep)


-- all lang deps atm
get_edges_from_rhs :: Node -> RHS -> [Edge]
get_edges_from_rhs s1 (RHS _ symList) = map (get_edge_from_symbols s1) symList


-- gets all associated edges for  a given rule
get_edges_from_rule :: Rule -> [Edge]
get_edges_from_rule (Rule sym rhsList) = intercalate [] (map (get_edges_from_rhs sym) rhsList)
