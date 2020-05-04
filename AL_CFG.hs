--
-- AL_CFG.hs
-- (Arbitrary Language Context-Free Grammar)
--
-- Used to describe a CFG that can be used to represent an arbitrary language
-- part of the DepGraph that can be produced later
--

module AL_CFG where

type Term     = String
type NonTerm  = String
type Comment  = String
type Description  = String

--
data Explanation = Expl Description

-- represents a non-terminating or terminating symbol
-- TODO enrich?
-- diff kinds of terms...some need explanations (operator names)
-- can't say exprs depend on ()...
data Symbol = NT NonTerm | T Term
  deriving (Show)

-- added
-- TODO Integer for what symbol in the list for this RHS defines a name
-- likely 0 in most cases (no symbol that defines a name)
-- so all existing rules define no names
-- add Let expr to arithmetic exprs, and integrate <---
-- Let x = e... (for usage deps)
-- boils down to modeling a given lang via this rep...
-- might be other things as well
--
data RHS = RHS Comment [Symbol]
  deriving (Show)

-- smart constructor for no comment
rhs :: [Symbol] -> RHS
rhs = RHS ""

-- represents a rule, with a name, composed of a list of symbols
-- a given rule can different substitutions
data Rule = Rule NonTerm [RHS]
  deriving (Show)

-- defines the grammar for an arbitrary language
data Grammar = Grammar String [Rule]
  deriving (Show)

-- pretty printer
pretty_print_symbols :: [Symbol] -> String
pretty_print_symbols []     = ""
pretty_print_symbols ((NT s):sl) = s ++ pretty_print_symbols sl
pretty_print_symbols ((T s):sl) = s ++ pretty_print_symbols sl

pretty_print_rule_parts :: [RHS] -> String
pretty_print_rule_parts []     = ""
pretty_print_rule_parts ((RHS comment symbols):(rule:rules)) = pretty_print_symbols symbols ++ "|" ++ pretty_print_rule_parts (rule:rules)
pretty_print_rule_parts ((RHS comment symbols):rules) = pretty_print_symbols symbols

pretty_print_rules :: [Rule] -> String
pretty_print_rules []     = ""
pretty_print_rules ((Rule nonterm rules):rulelist) = nonterm ++ ": " ++ pretty_print_rule_parts rules ++ "\n" ++ pretty_print_rules rulelist

-- TODO
-- ::=, space after Nonterminal
-- i is not terminal, NT (val)
-- meta-lang
-- nxt: rules for arithmetic grammar -> lang deps
-- first need def for lang dep (as defined below)
data LangDep = LD1 NonTerm Symbol | -- consider [] or not, usage dependent
  LD2 NonTerm [Symbol] -- maybe...

type LangDeps = [LangDep] -- more thoughts here...data or type

-- want Grammar -> LangDeps...
-- via grammar def structure
-- eliminate certain deps (like Number in Erwig's example)

-- usage deps trickier, need concept of what constitutes a name
--


pretty_print :: Grammar -> IO()
pretty_print (Grammar s rl) = putStr ("\n" ++ s ++ "\n--Rules--\n" ++ pretty_print_rules rl)
