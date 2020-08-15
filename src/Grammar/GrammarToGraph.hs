--
-- Converts a grammar to a graph
--

module Grammar.GrammarToGraph (GrammarDependency(LanguageDependency),grammar_to_graph) where

import Grammar.Symbol
import Grammar.Rule
import Grammar.Grammar
import Data.Set (toList,fromList)


-- | Defines grammar dependencies we can have
data GrammarDependency =
  LanguageDependency  -- ^ language dependency
  deriving (Show,Ord,Eq)


-- | Get symbols from the RHS of a grammar
getSymbolsFromRHS :: [RHS] -> [Symbol]
getSymbolsFromRHS [] = []
getSymbolsFromRHS ((RHS _ syms):rl) =  syms ++ getSymbolsFromRHS rl


-- | Gets all symbols from a grammar
_getSymbols :: [Rule] -> [Symbol]
_getSymbols [] = []
_getSymbols ((Rule s rhs):rl) = (s:(getSymbols rl)) ++ getSymbolsFromRHS rhs

-- | Removes duplicates from a list
makeUnique :: Ord a => [a] -> [a]
makeUnique = toList . fromList


-- | Removes deps that self reference (no recursion allowed)
removeRecursiveRefs :: [(GrammarDependency, Symbol, Symbol)] -> [(GrammarDependency, Symbol, Symbol)]
removeRecursiveRefs [] = []
removeRecursiveRefs (a@(gd,s1,s2):ls) | s1 /= s2 =  (a : (removeRecursiveRefs ls)) -- keep
                                      | s1 == s2 =  removeRecursiveRefs ls -- remove self reference


-- | Extract a list of symbols from a list of RHS rules
getSymbols :: [Rule] -> [Symbol]
getSymbols rules = makeUnique $ _getSymbols rules


-- | Extract all deps from a list of rules, no need to check for duplicates
getDeps :: [Rule] -> [(GrammarDependency, Symbol, Symbol)]
getDeps [] = []
getDeps ((Rule s _rhs):rl) = (map (\x -> (LanguageDependency,s,x)) (getSymbolsFromRHS _rhs)) ++ getDeps rl

-- | Converts a grammar to a graph,
-- which is compatible with our concept graph
grammar_to_graph :: Grammar -> ([Symbol], [(GrammarDependency,Symbol,Symbol)])
grammar_to_graph (Grammar _ rules) = (getSymbols rules, removeRecursiveRefs $ makeUnique $ getDeps rules)
