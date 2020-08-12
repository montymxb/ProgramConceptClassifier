--
-- BOGL_Lang.hs
--
-- Simplified representation of BOGL's syntax
--

import Symbol
import Rule
import Grammar
import GrammarToGraph
import GraphToConceptGraph
import ConceptGraph
import Query
import GVSpec

--
--
--
-- BoGL Syntax
--
--
--

-- Describes any bogl program
program = NonTerminal "Program" ["--bogl example\n"]

-- Body of a BoGL program, used when you want a full program, not just an expression
body = NonTerminal "Body" ["--body of a bogl program\n"]

-- game 'Name' syntactic elements
gameDecl = NonTerminal "GameDecl" ["game BoGLExample\n"]
gameKeyword = Terminal "game" ["game"]
gameName = Terminal "GameName" ["ExampleGame"]

-- board def can explicit or none
boardDef = NonTerminal "BoardDef" ["type Board = Array(1,1) of ",""]

-- input def can be explicit or none
inputDef = NonTerminal "InputDef" ["type Input = ",""]

-- any type syn
typeSyn = NonTerminal "TypeSyn" ["type TypeSyn = "]

-- any BoGL type (Int,Bool,Symbol,Board,etc.)
boglType = NonTerminal "Type" []

-- declaration of any various category
decl = NonTerminal "Decl" []

-- various equations that count as declarations
varEq = NonTerminal "VarEq" ["x : Int\nx = "]
funcEq = NonTerminal "FuncEq" ["succ : Int -> Int\nsucc(y) = "]
boardEq = NonTerminal "BoardEq" ["exampleBoard : Board\nexampleBoard!(x,y) = "]

-- any lowercase identifier that constitutes a name, bound function or reference
name = Terminal "Name" ["succ,x,exampleBoard"]

-- for identifiers
lowercaseWord = Terminal "Lowercase_Word" []
-- for types
uppercaseWord = Terminal "Uppercase_Word" []

-- keyword that preceeds a type synonym
typeKeyword = Terminal "type" ["type"]

-- any type name
typeName = Terminal "TypeName" []

-- BoGL Types
boardType = Terminal "Board" ["Board"]
inputType = Terminal "Input" ["Input"]
intType = Terminal "Int" ["Int"]
boolType = Terminal "Bool" ["Bool"]
playerType = Terminal "Player" ["Player"]
symbol = NonTerminal "Symbol" ["Symbol"]
tupleType = NonTerminal "Tuple" ["Tuple"]

-- assignment operator
assignment = Terminal "=" ["="]

-- declaration of an array, used for a Board
arrayDecl = NonTerminal "ArrayDecl" []
arrayType = Terminal "Array" ["Array"]
_of = Terminal "of" ["of"]

-- expressions, and their associated parts
expr = NonTerminal "Expression" []
binop = NonTerminal "BinOp" ["+","-","*","/","%","==","/=","<=",">=",">","<"]

-- position, a special type used to recognize inputs for board locations
position = NonTerminal "Position" ["(Int,Int)"]

-- binds a name to a value
colon = Terminal ":" []

-- distinguishes Input from Output
arrow = Terminal "->" []

-- Union of Sets
_and = Terminal "&" []

-- Array Get Operator
get = Terminal "!" []

-- Curly braces used to denote a set
ocurly = Terminal "{" []
ccurly = Terminal "}" []

-- Parentheses used to denote tuples of Types or Values
oparen = Terminal "(" []
cparen = Terminal ")" []

-- Binary Operators
plus = Terminal "+" ["5 + 2"]
minus = Terminal "-" ["5 - 2"]
times = Terminal "*" ["5 * 2"]
division = Terminal "/" ["5 / 2"]
modulo = Terminal "%" ["5 % 2"]

-- Let Exprs
let_in = NonTerminal "let-in" ["let x = 5 in x"]
_let = Terminal "let" []
_in = Terminal "in" []

-- IfThenElse Statements
if_then_else = NonTerminal "if-then-else" ["if True then 1 else 0","if False then 1 else 0"]
_if = Terminal "if" []
_then = Terminal "then" []
_else = Terminal "else" []

-- WhileDo Statements
while_do = NonTerminal "while-do" ["while (x < 10) do (x+1)"]
_while = Terminal "while" []
_do = Terminal "do" []

-- Denotes a set of Symbols
symbolSet = NonTerminal "Set_of_Symbols" ["{A,B,C}"]

-- denotes parameters for a function
parameters = NonTerminal "Parameters" ["(x,y)"]

-- denotes what separates statements
sep = Terminal "line-break" ["\n"] -- separates statements

--
--
--
-- BoGL Grammar Rules
--
--
--

-- Rule that denotes that a program is at least a game declaration
programR = Rule program [
  RHS "" [gameDecl]]

-- Body of a BoGL program is either an expression or a full program
bodyR = Rule body [
  RHS "-- a normal bogl program" [typeSyn,sep,boardDef,sep,inputDef,sep,decl],
  RHS "-- just an expression to run in the REPL" [expr]]

-- game declaration, followed by the body
gameDeclR = Rule gameDecl [
  RHS "name of a BoGL game" [gameKeyword,gameName,sep,body]]

-- possible board declaration
boardDefR = Rule boardDef [
  RHS "defines the size and type of the boards we can use" [typeKeyword,boardType,assignment,arrayDecl,sep],
  RHS "" []]

-- array decl for board
arrayDeclR = Rule arrayDecl [
  RHS "" [arrayType,position,_of,boglType]]

-- possible input declaration
inputDefR = Rule inputDef [
  RHS "tells BoGL what kind of input you may expect" [typeKeyword,inputType,assignment,boglType],
  RHS "" []]

-- a single type syn, or many type syns
typeSynR = Rule typeSyn [
  RHS "One Type Syn" [typeKeyword,typeName,assignment,boglType,sep],
  RHS "Many" [typeSyn,typeSyn]]

-- any kind of declaration (Var,Func,Board) as well as a 1+ TypeSyns and any other declarations
declR = Rule decl [
  RHS "Var Eq Decl" [name,colon,boglType,sep,varEq],
  RHS "Func Eq Decl" [name,colon,boglType,arrow,boglType,sep,funcEq],
  RHS "Board Eq Decl" [name,colon,boardType,sep,boardEq],
  RHS "TypeSyn" [typeSyn],
  RHS "Many Decl" [decl,decl]]

-- Any BoGL type, potentially more than 1
boglTypeR = Rule boglType [
  RHS "Bool" [boolType],
  RHS "Int" [intType],
  RHS "Board" [boardType],
  RHS "TypeSyn" [typeName],
  RHS "TypeExt" [typeName,_and,symbolSet],
  RHS "Tuple of Types" [tupleType],
  RHS "Many" [boglType,boglType]]

-- Set of symbols
symbolSetR = Rule symbolSet [
  RHS "Symbol Set" [ocurly,symbol,ccurly]]

-- Tuple Type
tupleTypeR = Rule tupleType [
  RHS "Tuple Decl" [oparen,cparen]]

-- Symbol can be 1 or more
symbolR = Rule symbol [
  RHS "One Uppercase Word" [uppercaseWord],
  RHS "Many" [symbol,symbol]]

-- Var equation
varEqR = Rule varEq [
  RHS "Var Eq" [name,assignment,expr]]

-- Params that are passed to a Function equation
parametersR = Rule parameters [
  RHS "Parameters" [oparen,name,cparen]]

-- declaring a function equation
funcEqR = Rule funcEq [
  RHS "Func Eq" [name,parameters,assignment,expr]]

-- declaring a board equation
boardEqR = Rule boardEq [
  RHS "Board Eq" [name,get,position,assignment,expr]]

-- denoting a rule for names
nameR = Rule name [
  RHS "One Lowercase name" [lowercaseWord],
  RHS "Many" [name,name]]

-- Various valid type names
typeNameR = Rule typeName [
  RHS "Int" [intType],
  RHS "Bool" [boolType],
  RHS "Board" [boardType],
  RHS "Player" [playerType],
  RHS "UppercaseWord" [uppercaseWord]]

-- Valid expressions
exprR = Rule expr [
  RHS "IntVal" [intType],
  RHS "BoolVal" [boolType],
  RHS "BoardVal" [boardType],
  RHS "Symbol" [symbol],
  RHS "Reference" [name],
  RHS "Get" [name,get,position],
  RHS "App" [name,oparen,expr,cparen],
  RHS "BinOp" [expr,binop,expr],
  RHS "Let" [let_in],
  RHS "If" [if_then_else],
  RHS "While" [while_do],
  RHS "Many" [expr,expr]]

-- Let Exprs
let_inR = Rule let_in [
  RHS "LetIn" [_let,name,assignment,_in]]

-- If then else statement
if_then_elseR = Rule if_then_else [
  RHS "IfThenElse" [_if,_then,_else]]

-- While-Do statement
while_doR = Rule while_do [
  RHS "WhileDo" [_while,_do]]

-- BVarious Binary Operators
binopR = Rule binop [
  RHS "Plus" [plus],
  RHS "Minus" [minus],
  RHS "Times" [times],
  RHS "Division" [division],
  RHS "Mod" [modulo]]

-- Position rule
positionR = Rule position [
  RHS "2D Position" [oparen,intType,intType,cparen]]

-- | Representation of the BoGL language as a grammar
bogl_grammar_rep :: Grammar
bogl_grammar_rep = Grammar "BOGL Lang" [
  programR,
  bodyR,
  gameDeclR,
  boardDefR,
  arrayDeclR,
  inputDefR,
  typeSynR,
  declR,
  boglTypeR,
  symbolSetR,
  tupleTypeR,
  symbolR,
  varEqR,
  parametersR,
  funcEqR,
  boardEqR,
  nameR,
  typeNameR,
  exprR,
  binopR,
  positionR]


-- | Produces a BoGL Concept Graph (of all concepts)
--
-- To see the entire concept graph
-- writeGVSpec "test1" bogl_concept_graph
--
bogl_concept_graph :: ConceptGraph GrammarDependency Symbol
bogl_concept_graph = graph_to_concept_graph (grammar_to_graph bogl_grammar_rep)


-- | Example Query, from one syntactic element to another
-- like: query name program...
-- gives all possible paths from program -> name
q1 :: Query Symbol
q1 = query let_in decl


-- | Takes a query object to perform a query on the BoGL concept graph
-- Produces a ConceptLattice
--
-- To see the ConceptLattice
-- writeGVSpec "conceptlattice_1" $ bogl_query q1
--
-- writeGVSpec "cl_3" $ queryFromKnownToGoal bogl_concept_graph [name] [boglType]
--
bogl_query :: Query Symbol -> ConceptLattice GrammarDependency Symbol
bogl_query q = queryGraph bogl_concept_graph q


-- | Gets all paths produced by querying the BoGL concept graph
--
-- > bogl_paths q1
-- > symbolsToExamples $ bogl_paths q1
--
bogl_paths :: Query Symbol -> [Maybe (Path Symbol)]
bogl_paths q = _queryGraph bogl_concept_graph q


-- | Just for debugging..
pall :: [String] -> IO ()
pall []     = putStrLn ""
pall (s:ls) = do
  putStrLn (s ++ "\n")
  pall ls

-- | List of example programs generated by the paths for a query on the BoGL concept graph
-- Can use the function above to separate them out a bit
-- > pall $ bogl_exs
bogl_exs :: [String]
bogl_exs = symbolsToExamples $ bogl_paths q1
