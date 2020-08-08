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

import TaggedSyntax

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


program = NonTerminal "Program" ["A BoGL Program"]
body = NonTerminal "Body" ["Body of a BoGL Program"]

gameDecl = NonTerminal "GameDecl" ["Declares a Game in BoGL"]
gameKeyword = Terminal "game" ["Game keyword"]
gameName = Terminal "GameName" ["Names a game in BoGL"]

boardDef = NonTerminal "BoardDef" ["Board Definition"]
inputDef = NonTerminal "InputDef" ["Input Definition"]
typeSyn = NonTerminal "TypeSyn" ["Type Synonym"]
decl = NonTerminal "Decl" ["Declaration"]
boglType = NonTerminal "Type" ["Type"]
varEq = NonTerminal "VarEq" ["Variable Equation"]
funcEq = NonTerminal "FuncEq" ["Function Equation"]
boardEq = NonTerminal "BoardEq" ["Board Equation"]
name = Terminal "Name" ["Name"]

lowercaseWord = Terminal "Lowercase_Word" ["Lowercase Identifier (names)"]
uppercaseWord = Terminal "Uppercase_Word" ["Uppercase Identifier (types)"]

typeKeyword = Terminal "type" ["Type Keyword"]
typeName = Terminal "TypeName" ["Type Name"]

boardType = Terminal "Board" ["Board Type"]
inputType = Terminal "Input" ["Input Type"]
intType = Terminal "Int" ["Int Type"]
boolType = Terminal "Bool" ["Bool Type"]
playerType = Terminal "Player" ["Player Type"]
symbol = NonTerminal "Symbol" ["Symbol"]
tupleType = NonTerminal "Tuple" ["Tuple Type"]

assignment = Terminal "=" ["Assignment"]

arrayDecl = NonTerminal "ArrayDecl" ["Array Declaration"]
arrayType = Terminal "Array" ["Array"]
_of = Terminal "of" ["Of"]

expr = NonTerminal "Expression" ["Expression"]
binop = NonTerminal "BinOp" ["Binary Operator"]
position = NonTerminal "Position" ["Position"]

colon = Terminal ":" ["Separates Name from Signature"]
arrow = Terminal "->" ["Distinguishes Input from Output"]
_and = Terminal "&" ["Union of Sets"]
get = Terminal "!" ["Array Get Operator"]

ocurly = Terminal "{" ["Open Curly Brace"]
ccurly = Terminal "}" ["Close Curly Brace"]

oparen = Terminal "(" ["Open Parentheses"]
cparen = Terminal ")" ["Close Parentheses"]

plus = Terminal "+" ["Plus"]
minus = Terminal "-" ["Minus"]
times = Terminal "*" ["Times"]
division = Terminal "/" ["Division"]
modulo = Terminal "%" ["Modulo"]

let_in = NonTerminal "let-in" ["let ... in ..."]
_let = Terminal "let" []
_in = Terminal "in" []

if_then_else = NonTerminal "if-then-else" ["if ... then ... else ..."]
_if = Terminal "if" []
_then = Terminal "then" []
_else = Terminal "else" []

while_do = NonTerminal "while-do" ["while ... do ..."]
_while = Terminal "while" []
_do = Terminal "do" []

symbolSet = NonTerminal "Set_of_Symbols" ["Set of Symbols"]

parameters = NonTerminal "Parameters" ["Parameters passed to a function"]

programR = Rule program [
  RHS "Start" [gameDecl]]

bodyR = Rule body [
  RHS "Body" [typeSyn,boardDef,inputDef,decl],
  RHS "Just an Expr" [expr]]

--programR = Rule program [
--  RHS "Full Program" [gameDecl,typeSyn,boardDef,inputDef,decl],
--  RHS "Just an Expr" [expr]]

gameDeclR = Rule gameDecl [
  RHS "Game Declaration" [gameKeyword,gameName,body]]

boardDefR = Rule boardDef [
  RHS "Explicit Board" [typeKeyword,boardType,assignment,arrayDecl],
  RHS "None" []]

arrayDeclR = Rule arrayDecl [
  RHS "Array(x,y) of Type" [arrayType,position,_of,boglType]]

inputDefR = Rule inputDef [
  RHS "Explicit Input" [typeKeyword,inputType,assignment,boglType],
  RHS "None" []]

typeSynR = Rule typeSyn [
  RHS "One Type Syn" [typeKeyword,typeName,assignment,boglType],
  RHS "Many" [typeSyn,typeSyn]]

declR = Rule decl [
  RHS "Var Eq Decl" [name,colon,boglType,varEq],
  RHS "Func Eq Decl" [name,colon,boglType,arrow,boglType,funcEq],
  RHS "Board Eq Decl" [name,colon,boardType,boardEq],
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
  RHS "2d Position" [oparen,intType,intType,cparen]]

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
q1 = query intType program

--
-- To see the ConceptLattice
-- writeGVSpec "conceptlattice_1" $ bogl_query q1
--
-- writeGVSpec "cl_3" $ queryFromKnownToGoal bogl_concept_graph [name] [boglType]
--
bogl_query :: Query Symbol -> ConceptLattice GrammarDependency Symbol
bogl_query q = queryGraph bogl_concept_graph q




--
-- Tagged BoGL Expr
--
tagged_expr = (Any [(TS "123" intValue)])

--
-- Tagged BoGL Body
--
--tagged_body = (All [] ...)

--
--
--
tagged_prog :: TaggedProgram Symbol
tagged_prog = (All [
  (TS "--example program" program),
  (TS "game Example" gameDecl)] (Any [(TS "--expr" tagged_expr)]))
