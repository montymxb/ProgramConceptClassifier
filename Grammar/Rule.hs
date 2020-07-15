--
-- Rules used to represent a grammar
--

module Rule (Rule(Rule),RHS(RHS),rhs) where


import Symbol


-- comment to describe a rule, if desired
type Comment = String


-- rhs for a rule
-- comment with list of symbols
data RHS = RHS Comment [Symbol]
  deriving (Show)


-- represents a rule, with a name, composed of a list of symbols
-- a given rule can express different substitutions
data Rule = Rule Symbol [RHS]
  deriving (Show)


-- RHS w/ no comment
rhs :: [Symbol] -> RHS
rhs = RHS ""
