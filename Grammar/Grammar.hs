--
-- Representation of a context-free-grammar for a language
--
-- Used to describe a CFG that can be used to represent an arbitrary language
--
module Grammar (Grammar(Grammar)) where

import Symbol
import Rule

import Data.Char
import Data.Set (toList,fromList)
import Data.List


type Name = String

-- defines the grammar for an arbitrary language
data Grammar = Grammar Name [Rule]
  deriving (Show)
