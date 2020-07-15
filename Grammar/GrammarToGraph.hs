--
-- Converts a grammar to a graph
--

module GrammarToGraph (GrammarDependency,grammar_to_graph) where

import Symbol
import Rule
import Grammar
import Data.Set (toList,fromList)


-- grammar dependencies
data GrammarDependency =
  LanguageDependency  -- language dependency
  deriving (Show,Ord,Eq)


-- get symbols from the RHS of a grammar
getSymbolsFromRHS :: [RHS] -> [Symbol]
getSymbolsFromRHS [] = []
getSymbolsFromRHS ((RHS _ syms):rl) =  syms ++ getSymbolsFromRHS rl


-- gets all symbols from a grammar
_getSymbols :: [Rule] -> [Symbol]
_getSymbols [] = []
_getSymbols ((Rule s rhs):rl) = (s:(getSymbols rl)) ++ getSymbolsFromRHS rhs

-- removes duplicates in this list
makeUnique :: Ord a => [a] -> [a]
makeUnique = toList . fromList


-- removes deps that self reference
removeRecursiveRefs :: [(GrammarDependency, Symbol, Symbol)] -> [(GrammarDependency, Symbol, Symbol)]
removeRecursiveRefs [] = []
removeRecursiveRefs (a@(gd,s1,s2):ls) | s1 /= s2 =  (a : (removeRecursiveRefs ls)) -- keep
                                      | s1 == s2 =  removeRecursiveRefs ls -- remove self reference


-- extract symbol from all RHS rules
getSymbols :: [Rule] -> [Symbol]
getSymbols rules = makeUnique $ _getSymbols rules


-- extract all deps from rules, no need to check for duplicates
getDeps :: [Rule] -> [(GrammarDependency, Symbol, Symbol)]
getDeps [] = []
getDeps ((Rule s _rhs):rl) = (map (\x -> (LanguageDependency,s,x)) (getSymbolsFromRHS _rhs)) ++ getDeps rl

-- converts a grammar to a graph,
-- which is compatible with our concept graph
grammar_to_graph :: Grammar -> ([Symbol], [(GrammarDependency,Symbol,Symbol)])
grammar_to_graph (Grammar _ rules) = (getSymbols rules, removeRecursiveRefs $ makeUnique $ getDeps rules)
