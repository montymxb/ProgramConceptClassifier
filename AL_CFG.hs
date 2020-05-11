--
-- AL_CFG.hs
-- (Arbitrary Language Context-Free Grammar)
--
-- Used to describe a CFG that can be used to represent an arbitrary language
-- part of the DepGraph that can be produced later
--
module AL_CFG where


import Data.Char
import Data.Set (toList,fromList)


--type Term     = String
type NonTerm  = String
type Comment  = String
type Description  = String


data Term =
  Term String | -- just a symbol
  Use String  | -- A usage of a var (terminal symbol)
  Def String    -- Definition of a var (terminal symbol)
  deriving (Show,Eq,Ord)

-- explanation for a term
-- Can be text, code, both, or none
data Explanation =
  Text Description |
  Code Description |
  TextAndCode Description Description |
  None
  deriving (Show,Eq,Ord)


-- symbol type
data SymbolType = NonTerminal | Terminal
  deriving (Show)

-- symbol type constructor
-- takes list of non-terms, and a symbol, producing a symbol type
symbolType :: [Symbol] -> Symbol -> SymbolType
-- symbol w/ expl
symbolType nts s@(SE (Term _) _)= if any (s==) nts then NonTerminal else Terminal
symbolType _ (SE _ _)           = Terminal

symbolType nts s@(S (Term _))   = if any (s==) nts then NonTerminal else Terminal
symbolType _ (S _)              = Terminal


-- represents a non-terminating or terminating symbol
-- diff kinds of terms...some need explanations (operator names)
-- what makes a good explanation, text, examples, or both?
-- provide Explanation for every symbol
-- can't say exprs depend on ()...
--
-- Redefined any symbols as a Term and an Explanation (which is optional)
-- Any NT or T is defined by the grammar.
-- Any symbol on the LHS of a rule is a NonTerminal symbol, otherwise it is a Terminal symbol
--
-- Adds in Use/Def to symbol (both akin to Terminal)
--
data Symbol =
  SE Term Explanation   | -- term w/ explanation
  S Term                -- term no explanation
  deriving (Show,Eq,Ord)

-- rhs for any rule
-- comment with list of symbols
data RHS = RHS Comment [Symbol]
  deriving (Show)

-- smart constructor for no comment
rhs :: [Symbol] -> RHS
rhs = RHS ""

-- represents a rule, with a name, composed of a list of symbols
-- a given rule can different substitutions
data Rule = Rule Symbol [RHS]
  deriving (Show)

-- defines the grammar for an arbitrary language
data Grammar = Grammar String [Rule]
  deriving (Show)


print_explanation :: Explanation -> String
print_explanation (Text t)          = "(" ++ t ++ ")"
print_explanation (Code c)          = "(" ++ c ++ ")"
print_explanation (TextAndCode t c) = "(" ++ t ++ "," ++ c ++ ")"
print_explanation None              = ""


-- toggles whether explanations are shown
showExplanation :: Bool
showExplanation = False


-- builds list of nonterminals from the grammar by building a list of LHS symbols
get_nonterminals :: [Rule] -> [Symbol]
get_nonterminals []     = []
get_nonterminals ((Rule sym _):rl) = (sym:(get_nonterminals rl))


print_term :: ([Char] -> [Char]) -> Term -> String
-- one or the other
print_term f (Term s) = f s
-- both Terminal symbols
print_term f (Use s)  = map toLower s
print_term f (Def s)  = map toLower s

print_nt_symbol :: Symbol -> String
-- symbol w/ explanation
print_nt_symbol (SE s e) = print_term (map toUpper) s ++ if showExplanation then " " ++ print_explanation e else ""
-- symbol w/ no explanation
print_nt_symbol (S s)  = print_term (map toUpper) s


print_t_symbol :: Symbol -> String
-- symbol w/ explanation
print_t_symbol (SE s e) = print_term (map toLower) s ++ if showExplanation then " " ++ print_explanation e else ""
-- symbol w/ no explanation
print_t_symbol (S s)  = print_term (map toLower) s


-- prints a symbol by whether it is an NT or not
print_symbol :: [Symbol] -> Symbol -> String
print_symbol nts s = if any (s==) nts then print_nt_symbol s else print_t_symbol s


-- pretty printer
pretty_print_symbols :: [Symbol] -> [Symbol] -> String
pretty_print_symbols _ []     = ""
pretty_print_symbols nts (s:sl) = print_symbol nts s ++ pretty_print_symbols nts sl


-- breaks apart the RHS parts for a given rule
pretty_print_rule_parts :: [Symbol] -> [RHS] -> String
pretty_print_rule_parts _ []     = ""
pretty_print_rule_parts nts ((RHS comment symbols):(rule:rules)) = pretty_print_symbols nts symbols ++ " | " ++ pretty_print_rule_parts nts (rule:rules)
pretty_print_rule_parts nts ((RHS comment symbols):rules) = pretty_print_symbols nts symbols


-- breaks apart each rule ina grammar
pretty_print_rules :: [Symbol] -> [Rule] -> String
pretty_print_rules _ []     = ""
pretty_print_rules nts ((Rule sym rules):rulelist) = print_nt_symbol sym ++ ": " ++ pretty_print_rule_parts nts rules ++ "\n" ++ pretty_print_rules nts rulelist


-- TODO
-- ::=, space after Nonterminal
-- i is not terminal, NT (val)
-- meta-lang
-- nxt: rules for arithmetic grammar -> lang deps
-- first need def for lang dep (as defined below)
data LangDep = LangDep Symbol [Symbol]


make_unique :: Ord a => [a] -> [a]
make_unique = toList . fromList


instance Show LangDep where
  show (LangDep s sl) = (print_nt_symbol s) ++ " <- " ++ concat (map print_t_symbol sl)

type LangDeps = [LangDep] -- more thoughts here...data or type

-- want Grammar -> LangDeps...
-- via grammar def structure
-- eliminate certain deps (like Number in Erwig's example)
-- usage deps trickier, need concept of what constitutes a name
--

getsymbol_deps :: [RHS] -> [Symbol]
getsymbol_deps [] = []
getsymbol_deps ((RHS _ syms):rhsl) = syms ++ getsymbol_deps rhsl

langdeps_rule :: Rule -> LangDep
-- filter to be unique entries, and remove recursive ones
langdeps_rule (Rule s rhs) = LangDep s (filter (s/=) (make_unique (getsymbol_deps rhs)))

-- for a given representation of a CFG for a language, derive lang deps
langdeps :: Grammar -> LangDeps
-- for every rule (an NT), add in each unique lang dep
langdeps (Grammar _ rules) = map langdeps_rule rules



pretty_print :: Grammar -> IO()
pretty_print (Grammar s rl) = putStr ("\n" ++ s ++ "\n--Rules--\n" ++ pretty_print_rules (get_nonterminals rl) rl)
