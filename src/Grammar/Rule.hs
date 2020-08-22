{-# LANGUAGE DeriveDataTypeable #-}
--
-- Rules used to represent a grammar
--

module Grammar.Rule (Rule(Rule),RHS(RHS),rhs) where


import Grammar.Symbol
import Data.Data


-- | Comment to describe a rule, if desired
type Comment = String


-- | RHS for a rule
-- comment with list of symbols
data RHS = RHS Comment [Symbol]
  deriving (Show,Data,Typeable)


-- | Represents a rule, with a name, composed of a list of symbols
-- a given rule can express different substitutions
data Rule = Rule Symbol [RHS]
  deriving (Show,Data,Typeable)


-- RHS w/ no comment
rhs :: [Symbol] -> RHS
rhs = RHS ""
