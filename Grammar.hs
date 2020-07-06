--
-- Grammar.hs
-- An arbitrary language context-free grammar representation
--
-- Used to describe a CFG that can be used to represent an arbitrary language
-- part of the DepGraph that can be produced later
--
module Grammar where

import Edge
import Rule
import Node

import Data.Char
import Data.Set (toList,fromList)
import Data.List


-- defines the grammar for an arbitrary language
data Grammar = Grammar String [Rule]
  deriving (Show)


-- builds list of nonterminals from the grammar by building a list of LHS symbols
get_nonterminals :: [Rule] -> [Node]
get_nonterminals []     = []
get_nonterminals ((Rule sym _):rl) = (sym:(get_nonterminals rl))


-- toggles whether explanations are shown
showExplanation :: Bool
showExplanation = False


-- prints a symbol, conditionally using a specified transformation function
print_term :: (String -> String) -> Symbol -> String
print_term f (Term s) = f s
print_term f (Use s)  = map toLower s
print_term f (Def s)  = map toLower s


print_nt_symbol :: Node -> String
print_nt_symbol (s,e,_) = print_term (map toUpper) s ++ if showExplanation then " " ++ concat (map print_explanation e) else ""


print_t_symbol :: Node -> String
print_t_symbol (s,e,_) = print_term (map toLower) s ++ if showExplanation then " " ++ (concat (map print_explanation e)) else ""


-- prints a symbol by whether it is an NT or not
print_symbol :: [Node] -> Node -> String
print_symbol nts s = if any (s==) nts then print_nt_symbol s else print_t_symbol s


-- pretty printer
pretty_print_symbols :: [Node] -> [Node] -> String
pretty_print_symbols _ []     = ""
pretty_print_symbols nts (s:sl) = print_symbol nts s ++ pretty_print_symbols nts sl


-- breaks apart the RHS parts for a given rule
pretty_print_rule_parts :: [Node] -> [RHS] -> String
pretty_print_rule_parts _ []     = ""
pretty_print_rule_parts nts ((RHS comment symbols):(rule:rules)) = pretty_print_symbols nts symbols ++ " | " ++ pretty_print_rule_parts nts (rule:rules)
pretty_print_rule_parts nts ((RHS comment symbols):rules) = pretty_print_symbols nts symbols


-- breaks apart each rule ina grammar
pretty_print_rules :: [Node] -> [Rule] -> String
pretty_print_rules _ []     = ""
pretty_print_rules nts ((Rule sym rules):rulelist) = print_nt_symbol sym ++ ": " ++ pretty_print_rule_parts nts rules ++ "\n" ++ pretty_print_rules nts rulelist


--
-- Edge Related classifications
--


-- removes duplicates in this list
make_unique :: Ord a => [a] -> [a]
make_unique = toList . fromList



-- convert a list of RHS elements to their raw symbols (used for filtering out non-unique entries)
getsymbol_deps :: [RHS] -> [Node]
getsymbol_deps [] = []
getsymbol_deps ((RHS _ syms):rhsl) = syms ++ getsymbol_deps rhsl


-- convert a rule to a language dependency
--langdeps_rule :: Rule -> LangDep
-- filter to be unique entries, and remove recursive ones
--langdeps_rule (Rule s rhs) = LangDep s (filter (s/=) (make_unique (getsymbol_deps rhs)))


-- for a given representation of a CFG for a language, derive lang deps
--langdeps :: Grammar -> LangDeps
-- for every rule (an NT), add in each unique lang dep
--langdeps (Grammar _ rules) = map langdeps_rule rules


--
-- Prepping of data
--


-- extracts nodes from a right hand side list
get_nodes_from_rhs :: RHS -> [Node]
get_nodes_from_rhs (RHS c syms) = syms


-- gets all kinds of nodes for a rule, regardless of whether they are terminal or non-terminal
get_nodes_from_rule :: Rule -> [Node]
get_nodes_from_rule (Rule s rhs)  = intercalate [s] (map get_nodes_from_rhs rhs)


updateDerivedSymbolData :: [Node] -> [Node]
updateDerivedSymbolData nlist = map (setNodeSymbolType nlist) nlist


-- Produces [Node] for all Symbols, with proper categorizing as Term/NonTerm
nodes :: Grammar -> [Node]
nodes (Grammar _ rules) = let nlist = make_unique (intercalate [] (map get_nodes_from_rule rules)) in updateDerivedSymbolData nlist


-- Produces [Edge]...(Symbol,Symbol,EdgeData), representing the data stored in the given node
edges :: Grammar -> [Edge]
edges (Grammar _ rules) = intercalate [] (map get_edges_from_rule rules)


--
-- Debugging for grammar
--


nodeToString :: Node -> String
nodeToString n = show n ++ "\n"


-- lists all the nodes present in a grammar, as a debuggable string
listNodes :: Grammar -> String
listNodes g@(Grammar s sl) = "\n  --Nodes--\n  " ++ let syms = nodes g in concat (map nodeToString syms) ++ "\n"


-- prints an edge on a line
edgeToString :: Edge -> String
edgeToString e = show e ++ "\n"


-- update all instances of nodes in a given edge
updateEdgeWithNodes :: [Node] -> Edge -> Edge
updateEdgeWithNodes [] e = e
updateEdgeWithNodes (n:nl) (n1,n2,ec) = if n1 == n then (n,n2,ec) else if n2 == n then (n1,n,ec) else (n1,n2,ec)


-- updates a list of edges with a given list of nodes
updateEdgeNodeData :: [Node] -> [Edge] -> [Edge]
updateEdgeNodeData [] e = e
updateEdgeNodeData nlist [] = []
updateEdgeNodeData nlist elist = map (updateEdgeWithNodes nlist) elist


-- lists all edges in a grammar, as a debugable string
listEdges :: Grammar -> String
listEdges g = "\n  --Edges--\n  " ++ let e = updateEdgeNodeData (nodes g) (edges g) in concat (map edgeToString e) ++ "\n"


-- lists all nodes & edges in a grammar, as a printed string
listAll :: Grammar -> IO()
listAll g@(Grammar s sl) = putStr ("\n  " ++ s ++ "\n" ++ (listNodes g) ++ (listEdges g) ++ "\n")


-- pretty prints a grammar's representation for seeing everything
pretty_print :: Grammar -> IO()
pretty_print (Grammar s rl) = putStr ("\n" ++ s ++ "\n--Rules--\n" ++ pretty_print_rules (get_nonterminals rl) rl)
