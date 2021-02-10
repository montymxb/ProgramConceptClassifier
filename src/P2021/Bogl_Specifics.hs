module P2021.Bogl_Specifics where

---
--- BoGL Specific Stuff
---

import qualified Language.Syntax as LS
--import qualified Language.Types as LT
import Text.Parsec.Pos
import P2021.General

import Parser.Parser
import Data.Data
import Data.Either

-- Previously used, but now unneeded
{-
deriving instance (Eq a) => Eq (Game a)
deriving instance Eq BoardDef
deriving instance Eq InputDef
deriving instance (Typeable a, Data a) => Data (Game a)
deriving instance Data BoardDef
deriving instance Data InputDef
deriving instance (Typeable a, Data a) => Data (ValDef a)
deriving instance Data Type
deriving instance Data Xtype
deriving instance Data Signature
deriving instance (Typeable a, Data a) => Data (Equation a)
deriving instance (Typeable a, Data a) => Data (BoardEq a)
deriving instance (Typeable a, Data a) => Data (Expr a)
deriving instance Data Parlist
deriving instance Data Pos
deriving instance Data Op
deriving instance Data Btype
deriving instance Data Ftype
-}
data AttributeConcept = NotEquiv
  | GreaterEqual
  | LessEqual
  | SymbolExpr
  | Equiv
  | Div
  | Let
  | Mult
  | Greater
  | Sub
  | Less
  | While
  | IfThenElse
  | Tuple
  | BoolExpr
  | ForAll
  | Index
  | BoardEquation
  | ValueEquation
  | Get
  | Game
  | Value
  | FunctionEquation
  | Type
  | App
  | BinOp
  | Add
  | Ref
  | IntType
  | BoardDef
  | InputDef
  | SymbolType
  | TupType
  | BoolType
  | Tru
  | Fls
  | PlainType
  | IntExpr
  | Name
  deriving (Eq,Show,Enum)

boglConceptMapping :: String -> Maybe AttributeConcept
boglConceptMapping "Top" = Just SymbolType
boglConceptMapping "NotEquiv" = Just NotEquiv
boglConceptMapping "Geq" = Just GreaterEqual
boglConceptMapping "Leq" = Just LessEqual
boglConceptMapping "S" = Just SymbolExpr
boglConceptMapping "Equiv" = Just Equiv
boglConceptMapping "Div" = Just Div
boglConceptMapping "Let" = Just Let
boglConceptMapping "Times" = Just Mult
boglConceptMapping "Greater" = Just Greater
boglConceptMapping "Minus" = Just Sub
boglConceptMapping "Less" = Just Less
boglConceptMapping "While" = Just While
boglConceptMapping "If" = Just IfThenElse
boglConceptMapping "Tup" = Just TupType
boglConceptMapping "Booltype" = Just BoolType
-- [Xtype] ~ a list of Xtypes (n)
boglConceptMapping "True" = Just Tru
-- B ~ constructor for expression that evaluates to a Boolean, should probably have this? (n)
boglConceptMapping "B" = Just BoolExpr
boglConceptMapping "False" = Just Fls
-- Bool ~ not needed, as we will always have 'B' above when this is here too (the type of the value in B)
boglConceptMapping "ForAll" = Just ForAll
-- BVal ~ a Board Value (n), doesn't add anything new
-- Board ~ subsumed by BoardEq
-- PosDef ~ instance of a Board Equation... (n)
boglConceptMapping "Index" = Just Index
-- Pos ~ no help (n)
boglConceptMapping "BoardEq SourcePos" = Just BoardEquation
-- [BoardEq SourcePos] ~ array of board equations, which are the same as array of PosDef (n)
boglConceptMapping "Plain" = Just PlainType
boglConceptMapping "Veq" = Just ValueEquation
boglConceptMapping "Get" = Just Get
boglConceptMapping "Tuple" = Just Tuple
-- [Expr SourcePos] ~ List of expressions (n)
-- Game SourcePos ~ Annotated Game (n)
boglConceptMapping "Game" = Just Game
-- (,) ~ Tuple constructor (n)
-- (Int,Int) ~ Tuple of Ints constructor (n)
boglConceptMapping "Val" = Just Value
-- Sig ~ doesn't give us anything new
-- Function ~ Feq has this
-- Ft ~ Function type (n), does not add anything
-- X ~ Xtype (n)
-- Itype ~ instance of a base type (Btype) (n)
-- fromList ~ has to do with enums for enumerated types & board defs (which always have this interestingly enough...) (n)
-- Btype ~ Atomic type, includes (Booltype, Itype, AnySymbol, Input, Board, Top, Undef) (n)
-- Set [Char] ~ Used for Symbols I think (n)
-- Xtype ~ Sum type, X Tup or Hole (n)
-- Ftype ~ Function Type, plain type -> plain type (implied by functions) (n)
boglConceptMapping "Type" = Just Type
boglConceptMapping "Feq" = Just FunctionEquation
-- Pars ~ parameters, nothing added
-- [[Char]] ~ List of names, probably for symbols & or other thing (n)
boglConceptMapping "App" = Just App
-- BinOp ~ does not add anything 'Op' already does
boglConceptMapping "Plus" = Just Add
boglConceptMapping "Ref" = Just Ref
-- Annotation ~ used for annotating for parsing (n)
boglConceptMapping "I" = Just IntExpr
boglConceptMapping "Op" = Just BinOp
-- Parlist ~ list of names to bind to funtion equation parameters (n)
-- Expr SourcePos ~ nothing lost here
-- (:) ~ cons (n)
-- Char ~ single char (n)
boglConceptMapping "Int" = Just IntType
-- Signature ~ general signature (n)
-- Equation SourcePos ~ general equation (n)
-- SourcePos ~ annotation stuff (n)
-- [] ~ empty list (n)
-- ValDef SourcePos ~ value definition (n)
boglConceptMapping "[Char]" = Just Name
boglConceptMapping "BoardDef" = Just BoardDef
boglConceptMapping "InputDef" = Just InputDef
-- [ValDef SourcePos] ~ list of valdefs, (n)
boglConceptMapping x = Nothing


-- 1) Function that parses 2 lists of BoGL progs (from strings) into ASTs (nothing else yet)
parseBOGLPrograms :: [ConcreteProgram] -> [(String, LS.Game SourcePos)]
parseBOGLPrograms ls = fdown $ map (\(n,p) -> (n, parsePreludeAndGameText "" p "test")) ls

-- filter down programs that pass
fdown :: [(String,Either a (LS.Game SourcePos))] -> [(String,LS.Game SourcePos)]
fdown [] = []
fdown ((n,x):ls) = case x of
                      Left _  -> error $ "Failed to parse program '" ++ n ++ "'" --fdown ls
                      Right g -> (n,g) : fdown ls

exCP1 :: [ConcreteProgram]
exCP1 = [("OnlyProgram","game Simplest")]

exCP2 :: [ConcreteProgram]
exCP2 = [("Prog1","game Simplest"),
        ("Prog2","game Simplest\nv : Int\nv = 1"),
        ("Prog3","game Simplest\nv : Int\nv = 1 + 2")]

exCP3 :: [ConcreteProgram]
exCP3 = [("BasicProg","game S"),
        ("TypeDecl1","game S\ntype Number = Int"),
        ("Add","game S\ntype Number = Int\nv : Int\nv = 1 + 1"),
        ("Sub","game S\ntype Number = Int\nv : Int\nv = 1 - 1"),
        ("Let","game S\nv : Int\nv = let x = 2 in x"),
        ("LetAgain","game S\nv : Int\nv = let x = 2 in let y = 3 in x")]

exCP4 :: [ConcreteProgram]
exCP4 = [("Base","game S"),
        ("Val","game S\nv : Int\nv = 1"),
        ("Add","game S\ntype Number = Int\nv : Int\nv = 1 + 1"),
        ("Sub","game S\ntype Number = Int\nv : Int\nv = 1 - 1"),
        ("Let","game S\nv : Int\nv = let x = 2 in x"),
        ("LetAddSub","game S\nv : Int\nv = let x = 2 in let y = 3 in x + y - 1"),
        ("AddSubFunc","game S\nadd : (Int,Int) -> Int\nadd(x,y) = let z = 1 in x+y - z")]

exCP5 :: [ConcreteProgram]
exCP5 = [("A","game A"),
        ("B","game B\nv : Int\nv = 32"),
        ("C","game C\nv : Int\nv = 2 + 1"),
        ("D","game D\nv : Int\nv = 2 * 4"),
        ("E","game E\nv : Int\nv = 2 + 5 * 2")]

exConcretePrograms :: [ConcreteProgram]
exConcretePrograms = [("Simplest","game Simplest"),
  ("V_Int","game E\nv : Int\nv = 0"),
  ("V_Bool","game E\nv : Bool\nv = True"),
  ("V_Sym1","game E\ntype T = {A,B}\nv : T\nv = A"),
  ("V_Sym2","game E\ntype T = {A,B}\nf : T -> T\nf(x) = if x == A then B else A"),
  ("V_Sym3","game E\ntype T = {A,B}\nf : T -> T\nf(x) = if x /= A then B else A"),
  ("V_Tup1","game E\nv : (Int,Int)\nv = (1,2)"),
  ("V_Tup2","game E\nv : (Bool,Bool)\nv = (True,False)"),
  ("F_Tup1","game E\nf : Int -> (Int,Int)\nf(x) = (x,x)"),
  ("V_Add","game E\nv : Int\nv = 1 + 1"),
  ("V_Sub","game E\nv : Int\nv = 1 - 1"),
  ("V_Mul","game E\nv : Int\nv = 2 * 5"),
  ("V_Div","game E\nv : Int\nv = 10 / 2"),
  ("V_AddSub","game E\nv : Int\nv = 1 + 2 - 1"),
  ("V_AddMul","game E\nv : Int\nv = 1 + 2 * 5"),
  ("V_AddSubMulDiv","game E\nv : Int\nv = 1 + 2 - 1 * 5 / 2"),
  ("V_CondT","game E\nv : Bool\nv = True == True"),
  ("V_CondF","game E\nv : Bool\nv = False == False"),
  ("V_CondMix1","game E\nv : Bool\nv = False == True"),
  ("V_CondMix2","game E\nv : Bool\nv = True == False"),
  ("V_CondNe1","game E\nv : Bool\nv = True /= False"),
  ("V_CondNe2","game E\nv : Bool\nv = True /= True"),
  ("V_CondNeNum","game E\nv : Bool\nv = 1 /= 2"),
  ("V_CondEqNum","game E\nv : Bool\nv = 5 == 2"),
  ("V_CondLt","game E\nv : Bool\nv = 5 < 2"),
  ("V_CondGte","game E\nv : Bool\nv = 5 >= 2"),
  ("V_CondLte","game E\nv : Bool\nv = 5 <= 2"),
  ("V_CondGt","game E\nv : Bool\nv = 5 > 2"),
  ("V_ITE1","game E\nv : Int\nv = if True then 1 else 0"),
  ("V_ITE4","game E\nv : Int\nv = if False then 1 else 0"),
  ("V_ITE2","game E\nv : Bool\nv = if 1 == 1 then True else False"),
  ("V_ITE3","game E\nv : Bool\nv = if True == False then True else False"),
  ("V_Let1","game E\nv : Int\nv = let x = 5 in x"),
  ("V_Let2","game E\nv : Int\nv = let x = 5 * 2 in x"),
  ("V_Let3","game E\nv : Int\nv = let x = 5 + 1 * 2 - 3 in x"),
  ("V_Let4","game E\nv : Bool\nv = let x = 5 + 1 * 2 - 3 in x == 0"),
  ("V_Let5","game E\nv : Bool\nv = let x = True in False"),
  ("V_LetB1","game E\nv : Bool\nv = let x = True in x"),
  ("V_LetBrd","game E\ntype Board = Array(1,1) of Int\nb : Board\nb!(x,y) = let q = 5 in q"),
  ("V_And1","game E\nv : Bool\nv = and(1 == 1, 0 == 0)"),
  ("V_And2","game E\nv : Bool\nv = and(1 == 1, True == False)"),
  --("V_Or1","game E\nv : Bool\nv = or(1 == 1, True == False)"),
  ("V_Or1","game E\ntype T={A,B}\nv : Bool\nv = or(A == B, True == False)"),
  ("V_Not1","game E\nv : Bool\nv = not(True)"),
  ("V_Wh1","game E\nw : Int\nw = let z = 10 in while z > 0 do z - 1"),
  ("V_Wh2","game E\nw : Bool\nw = let z = True in while False do z"),
  ("V_Ref","game E\na : Int\na = 5\nb : Int\nb = a"),
  ("F_ref","game E\na : Int\na = 5\nb : Int -> Int\nb(x) = x + a"),
  ("F_always5","game E\na5 : Int -> Int\na5(x) = 5"),
  ("F_id","game E\nid : Int -> Int\nid(x) = x"),
  ("F_inc","game E\ninc : Int -> Int\ninc(x) = x+1"),
  ("F_dec","game E\ndec : Int -> Int\ndec(x) = x-1"),
  ("F_double","game E\ndub : Int -> Int\ndub(x) = x*2"),
  ("F_halve","game E\nhalve : Int -> Int\nhalve(x) = x/2"),
  ("F_gt","game E\ngt : (Int,Int) -> Bool\ngt(x,y) = x > y"),
  ("F_lt","game E\nlt : (Int,Int) -> Bool\nlt(x,y) = x < y"),
  ("F_not","game E\nnt : Bool -> Bool\nnt(x) = if x then False else True"),
  ("F_add","game E\nadd : (Int,Int) -> Int\nadd(x,y) = x+y"),
  ("F_sub","game E\nsub : (Int,Int) -> Int\nsub(x,y) = x-y"),
  ("F_mul","game E\nmul : (Int,Int) -> Int\nmul(x,y) = x*y"),
  ("F_div","game E\ndiv : (Int,Int) -> Int\ndiv(x,y) = x/y"),
  ("F_b","game E\nb : ((Bool,Bool),Bool) -> Bool\nb(x,y) = y"),
  --("F_let","game E\nflet : Int -> Int\nflet(x) = let q = 2 in x + q"),
  ("F_dublet","game E\ndlet : Int -> Int\ndlet(x) = let y = 2 in let z = 2 in x+y+z"),
  ("F_fact","game E\nfact : Int -> Int\nfact(x) = if x > 1 then x * fact(x-1) else 1"),
  ("F_wh1","game E\nwh : Int -> Int\nwh(x) = while x > 0 do x - 1"),
  ("F_wh2","game E\nwh : Int -> Int\nwh(x) = while x < 10 do x + 1"),
  ("F_wh3","game E\nwh : Int -> Int\nwh(x) = while False do x"),
  --("F_incWithType","game B2\ntype Pos = (Int,Int)\ninc : Int -> Int\ninc(x) = x + 1"),
  --("TupType","game T\ntype X = (Int,Int)"),
  --("ExtType","game T\ntype Ex = Int & {A,B}"),
  ("Input1","game T\ntype Input = Int\nv : Int\nv = input + 1"),
  --("BoardType","game B\ntype Board = Array(3,3) of Int"),
  ("Board1","game B\ntype Board = Array(3,3) of Int\nb : Board\nb!(x,y) = 1"),
  ("BoardIf","game B\ntype Board = Array(3,3) of Int\nb : Board\nb!(x,y) = if True then 1 else 0"),
  ("Board2","game B\ntype Board = Array(3,3) of Int\nb : Board\nb!(x,y) = 1\nb!(2,2) = 0"),
  ("Board3","game B\ntype Board = Array(1,1) of Int\nb : Board\nb!(1,1) = 8"),
  ("Board4","game B\ntype Board = Array(1,1) of Bool\nb : Board\nb!(1,1) = False"),
  ("Board5","game B\ntype Board = Array(1,1) of Bool\nb : Board\nb!(x,y) = True"),
  ("Board6","game B\ntype Board = Array(1,1) of (Bool,Bool)\nb : Board\nb!(x,y) = (True,False)"),
  ("Get1","game B\ntype Board = Array(1,1) of Int\nb : Board\nb!(x,y) = 5\nv : Int\nv = b!(1,1)"),
  ("Get2","game B\ntype Board = Array(1,1) of Int\nb : Board\nb!(1,1) = 8\nv : Int\nv = b!(1,1)"),
  ("InfinRecur","game E\nr : Int -> Int\nr(x) = r(x+1)")]
