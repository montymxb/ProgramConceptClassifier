--
-- Node.hs
--
-- Holds node related data
--

module Node where


import Data.Char


-- symbols are lists of characters
data Symbol =
  Term String | -- just a symbol
  Use String  | -- A usage of a var (terminal symbol)
  Def String    -- Definition of a var (terminal symbol)
  deriving (Show,Eq,Ord)


-- explanation for a symbol
data Explanation =
  TextAndCode String | -- text and code explanation as one item, otherwise, none
  None -- no explanation, which is valid
  deriving (Show,Eq,Ord)


-- symbol type
-- Can be NT, Term, or Unclassified (not analyzed yet for the grammar)
data SymbolType = NonTerminal | Terminal | Unclassified
  deriving (Show,Eq,Ord)


-- symbol data that is derived from the grammar structure
-- built up internally
type DerivedSymbolData = (SymbolType)


-- a node represents a symbol
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
type Node = (Symbol, [Explanation], DerivedSymbolData)


-- Converts an explanation to a string
print_explanation :: Explanation -> String
print_explanation (TextAndCode tandc) = "(" ++ tandc ++ ")"
print_explanation None                = ""


upperSymbol :: Symbol -> Symbol
upperSymbol (Term s)  = (Term $ map toUpper s)
upperSymbol (Use s)   = (Use $ map toUpper s)
upperSymbol (Def s)   = (Def $ map toUpper s)


lowerSymbol :: Symbol -> Symbol
lowerSymbol (Term s)  = (Term $ map toLower s)
lowerSymbol (Use s)   = (Use $ map toLower s)
lowerSymbol (Def s)   = (Def $ map toLower s)


-- updates the printable symbol
setNodeSymbolType :: [Node] -> Node -> Node
setNodeSymbolType nts n@(s,elist,_) = case (symbolType nts n) of
  NonTerminal -> (upperSymbol s, elist, NonTerminal)
  Terminal    -> (lowerSymbol s, elist, Terminal)
  nst         -> error ("Unable to set node symbol type of " ++ show nst)


-- symbol type constructor
-- takes list of non-terms, and a symbol, producing a symbol type
symbolType :: [Node] -> Node -> SymbolType
-- symbol w/ expl
symbolType nts s@((Term _),_,_)= if any (s==) nts then NonTerminal else Terminal
symbolType _ (_,_,_)           = Terminal


-- produces an unclassified symbol for easy setup
sym :: Symbol -> [Explanation] -> Node
sym t es = (t,es,(Unclassified))
