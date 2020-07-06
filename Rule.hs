--
-- Rule.hs
--

module Rule where

import Node

type Comment = String

-- rhs for any rule
-- comment with list of symbols
data RHS = RHS Comment [Node]
  deriving (Show)


-- smart constructor for no comment
rhs :: [Node] -> RHS
rhs = RHS ""


-- represents a rule, with a name, composed of a list of symbols
-- a given rule can different substitutions
data Rule = Rule Node [RHS]
  deriving (Show)
