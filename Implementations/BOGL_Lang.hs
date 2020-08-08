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

{-
program -> gameName,typeSyns,boardDef,inputDef,decls | expr --ehhh...more or less correct
gameName -> 'game',name
typeSyns -> typeSyn,typeSyns | []
boardDef -> boardDecl | []
inputDef -> inputDecl | []
decls -> decl,decls | typeSyn,decls | []
typeSyn -> 'type',name,'=',boglType
boardDecl -> 'type','Board','=',arrayDecl
inputDecl -> 'type','Input','=',boglType
decl -> name,':',boglType,varEq | name,':',boglType,'->',boglType,funcEq | name,':','Board',boardEq
boglType -> typeName,'&','{',symbol,symbols,'}' | '(',boglType,boglTypes,')' | typeName
boglTypes -> boglType,boglTypes | []
varEq -> name,'=',expr
funcEq -> name,'(',name,names,')','=',expr
boardEq -> name,'!',position,'=',expr
names -> name,names | []
name -> lowercaseWord
typeName -> Int | Bool | Board | Input | Player | UppercaseWord
exprs -> expr,exprs | []
expr -> intVal |
      boolVal |
      boardVal |
      symbol |
      reference |
      name,'!',position | -- get
      app,'(',expr,exprs,')' |
      expr,operator,expr |
      'let',name,'=',expr,'in',expr |
      'if',expr,'then',expr,'else',expr |
      'while',expr,'do',expr
operator -> + | - | * | /
position -> '(',Int,Int,')'
-}


program = NonTerminal "Program" ["--bogl example\n"]
body = NonTerminal "Body" ["--body of a bogl program\n"]

gameDecl = NonTerminal "GameDecl" ["game BoGLExample\n"]
gameKeyword = Terminal "game" ["game"]
gameName = Terminal "GameName" ["ExampleGame"]

boardDef = NonTerminal "BoardDef" ["type Board = Array(1,1) of ",""]
inputDef = NonTerminal "InputDef" ["type Input = ",""]
typeSyn = NonTerminal "TypeSyn" ["type TypeSyn = "]
decl = NonTerminal "Decl" []
boglType = NonTerminal "Type" []
varEq = NonTerminal "VarEq" ["x : Int\nx = "]
funcEq = NonTerminal "FuncEq" ["succ : Int -> Int\nsucc(y) = "]
boardEq = NonTerminal "BoardEq" ["exampleBoard : Board\nexampleBoard!(x,y) = "]
name = Terminal "Name" ["succ,x,exampleBoard"]

lowercaseWord = Terminal "Lowercase_Word" [] -- for identifiers
uppercaseWord = Terminal "Uppercase_Word" [] -- for types

typeKeyword = Terminal "type" ["type"]
typeName = Terminal "TypeName" []

boardType = Terminal "Board" ["Board"]
inputType = Terminal "Input" ["Input"]
intType = Terminal "Int" ["Int"]
boolType = Terminal "Bool" ["Bool"]
playerType = Terminal "Player" ["Player"]
symbol = NonTerminal "Symbol" ["Symbol"]
tupleType = NonTerminal "Tuple" ["Tuple"]

assignment = Terminal "=" ["="]

arrayDecl = NonTerminal "ArrayDecl" []
arrayType = Terminal "Array" ["Array"]
_of = Terminal "of" ["of"]

expr = NonTerminal "Expression" []
binop = NonTerminal "BinOp" ["+","-","*","/","%","==","/=","<=",">=",">","<"]
position = NonTerminal "Position" ["(Int,Int)"]

colon = Terminal ":" [] -- separates name from type in function/var signature
arrow = Terminal "->" [] -- Distinguishes Input from Output
_and = Terminal "&" [] -- Union of Sets
get = Terminal "!" [] -- Array Get Operator

ocurly = Terminal "{" []
ccurly = Terminal "}" []

oparen = Terminal "(" []
cparen = Terminal ")" []

plus = Terminal "+" ["5 + 2"]
minus = Terminal "-" ["5 - 2"]
times = Terminal "*" ["5 * 2"]
division = Terminal "/" ["5 / 2"]
modulo = Terminal "%" ["5 % 2"]

let_in = NonTerminal "let-in" ["let x = 5 in x"]
_let = Terminal "let" []
_in = Terminal "in" []

if_then_else = NonTerminal "if-then-else" ["if True then 1 else 0","if False then 1 else 0"]
_if = Terminal "if" []
_then = Terminal "then" []
_else = Terminal "else" []

while_do = NonTerminal "while-do" ["while (x < 10) do (x+1)"]
_while = Terminal "while" []
_do = Terminal "do" []

symbolSet = NonTerminal "Set_of_Symbols" ["{A,B,C}"]

parameters = NonTerminal "Parameters" ["(x,y)"]

sep = Terminal "line-break" ["\n"] -- separates statements

programR = Rule program [
  RHS "" [gameDecl]]

bodyR = Rule body [
  RHS "-- a normal bogl program" [typeSyn,sep,boardDef,sep,inputDef,sep,decl],
  RHS "-- just an expression to run in the REPL" [expr]]

gameDeclR = Rule gameDecl [
  RHS "name of a BoGL game" [gameKeyword,gameName,sep,body]]

boardDefR = Rule boardDef [
  RHS "defines the size and type of the boards we can use" [typeKeyword,boardType,assignment,arrayDecl,sep],
  RHS "" []]

arrayDeclR = Rule arrayDecl [
  RHS "" [arrayType,position,_of,boglType]]

inputDefR = Rule inputDef [
  RHS "tells BoGL what kind of input you may expect" [typeKeyword,inputType,assignment,boglType],
  RHS "" []]

typeSynR = Rule typeSyn [
  RHS "One Type Syn" [typeKeyword,typeName,assignment,boglType,sep],
  RHS "Many" [typeSyn,typeSyn]]

declR = Rule decl [
  RHS "Var Eq Decl" [name,colon,boglType,sep,varEq],
  RHS "Func Eq Decl" [name,colon,boglType,arrow,boglType,sep,funcEq],
  RHS "Board Eq Decl" [name,colon,boardType,sep,boardEq],
  RHS "TypeSyn" [typeSyn],
  RHS "Many Decl" [decl,decl]]

boglTypeR = Rule boglType [
  RHS "Bool" [boolType],
  RHS "Int" [intType],
  RHS "Board" [boardType],
  RHS "TypeSyn" [typeName],
  RHS "TypeExt" [typeName,_and,symbolSet],
  RHS "Tuple of Types" [tupleType],
  RHS "Many" [boglType,boglType]]

symbolSetR = Rule symbolSet [
  RHS "Symbol Set" [ocurly,symbol,ccurly]]

tupleTypeR = Rule tupleType [
  RHS "Tuple Decl" [oparen,cparen]]

symbolR = Rule symbol [
  RHS "One Uppercase Word" [uppercaseWord],
  RHS "Many" [symbol,symbol]]

varEqR = Rule varEq [
  RHS "Var Eq" [name,assignment,expr]]

parametersR = Rule parameters [
  RHS "Parameters" [oparen,name,cparen]]

funcEqR = Rule funcEq [
  RHS "Func Eq" [name,parameters,assignment,expr]]

boardEqR = Rule boardEq [
  RHS "Board Eq" [name,get,position,assignment,expr]]

nameR = Rule name [
  RHS "One Lowercase name" [lowercaseWord],
  RHS "Many" [name,name]]

typeNameR = Rule typeName [
  RHS "Int" [intType],
  RHS "Bool" [boolType],
  RHS "Board" [boardType],
  RHS "Player" [playerType],
  RHS "UppercaseWord" [uppercaseWord]]

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

let_inR = Rule let_in [
  RHS "LetIn" [_let,name,assignment,_in]]

if_then_elseR = Rule if_then_else [
  RHS "IfThenElse" [_if,_then,_else]]

while_doR = Rule while_do [
  RHS "WhileDo" [_while,_do]]

binopR = Rule binop [
  RHS "Plus" [plus],
  RHS "Minus" [minus],
  RHS "Times" [times],
  RHS "Division" [division],
  RHS "Mod" [modulo]]

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
  positionR,
  let_inR,
  if_then_elseR,
  while_doR]

--
-- To see the entire concept graph
-- writeGVSpec "test1" bogl_concept_graph
--
bogl_concept_graph :: ConceptGraph GrammarDependency Symbol
bogl_concept_graph = graph_to_concept_graph (grammar_to_graph bogl_grammar_rep)

-- like: query name program...
-- gives all possible paths from program -> name
q1 :: Query Symbol
q1 = query let_in decl

--
-- To see the ConceptLattice
-- writeGVSpec "conceptlattice_1" $ bogl_query q1
--
-- writeGVSpec "cl_3" $ queryFromKnownToGoal bogl_concept_graph [name] [boglType]
--
bogl_query :: Query Symbol -> ConceptLattice GrammarDependency Symbol
bogl_query q = queryGraph bogl_concept_graph q

--
-- > bogl_paths q1
-- > symbolsToExamples $ bogl_paths q1
--
bogl_paths :: Query Symbol -> [Maybe (Path Symbol)]
bogl_paths q = _queryGraph bogl_concept_graph q


pall :: [String] -> IO ()
pall []     = putStrLn ""
pall (s:ls) = do
  putStrLn (s ++ "\n")
  pall ls

bogl_exs = symbolsToExamples $ bogl_paths q1
