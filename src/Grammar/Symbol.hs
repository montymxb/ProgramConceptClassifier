--
-- Symbol in a grammar
--

module Grammar.Symbol (Symbol(Terminal,NonTerminal),terminal,nonterminal,symbolsToExamples) where


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

symbolsToExamples :: [Maybe [Symbol]] -> [String]
symbolsToExamples ms = map toExamples ms

toExamples :: Maybe [Symbol] -> String
toExamples (Just syms) = concat $ map toExample syms
toExamples Nothing     = ""

toExample :: Symbol -> String
-- no examples
toExample (NonTerminal _ [])       = ""
toExample (Terminal _ [])          = ""
-- use the 1st example
toExample (NonTerminal _ (ex:ls))  = ex
toExample (Terminal _ (ex:ls))     = ex
