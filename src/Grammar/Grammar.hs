{-# LANGUAGE DeriveDataTypeable #-}
--
-- Representation of a context-free-grammar for a language
--
-- Used to describe a CFG that can be used to represent an arbitrary language
--
module Grammar.Grammar (Grammar(Grammar)) where

import Grammar.Symbol
import Grammar.Rule

import Data.Char
import Data.Set (toList,fromList)
import Data.List
import Data.Data

-- | The name to associate with a representation of a grammar
type Name = String

-- | Defines a representation of a grammar that can represent a language
data Grammar = Grammar Name [Rule]
  deriving (Show,Data,Typeable)
