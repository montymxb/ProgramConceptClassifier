--
-- Symbol in a grammar
--

module Symbol (Symbol(Terminal,NonTerminal),terminal,nonterminal) where


-- name of this symbol
type SymbolName = String


-- text, code, pseudocode, w/e may be considered a decent explanation
type Explanation = String


-- Term or NonTerm symbol
data Symbol =
  Terminal SymbolName [Explanation]   | -- terminal symbol
  NonTerminal SymbolName [Explanation]  -- non-terminal symbol
  deriving (Show,Eq,Ord)


-- constructs terminal symbol w/out explanation
terminal :: SymbolName -> Symbol
terminal n = Terminal n []


-- constructs nonterm symbol w/out explanation
nonterminal :: SymbolName -> Symbol
nonterminal n = NonTerminal n []
