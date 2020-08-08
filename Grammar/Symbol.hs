--
-- Symbol in a grammar
--

module Symbol (Symbol(Terminal,NonTerminal),terminal,nonterminal) where


-- name of this symbol
type SymbolName = String


-- text, code, pseudocode, w/e may be considered a decent explanation
type Explanation = String


-- | Defines types of symbols we can use in our grammar
data Symbol =
  Terminal SymbolName [Explanation]   | -- ^ terminal symbol
  NonTerminal SymbolName [Explanation]  -- ^ non-terminal symbol
  deriving (Eq,Ord)

instance Show Symbol where
  show (Terminal sn _) = sn
  show (NonTerminal sn _ ) = sn


-- | Constructs terminal symbol w/out explanation
terminal :: SymbolName -> Symbol
terminal n = Terminal n []


-- | Constructs non-term symbol w/out explanation
nonterminal :: SymbolName -> Symbol
nonterminal n = NonTerminal n []
