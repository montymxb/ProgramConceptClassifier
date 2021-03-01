{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Bogl_Specifics where

---
--- BoGL Specific Stuff
---

import qualified Language.Syntax as LS
import Language.Types
import Text.Parsec.Pos
import General

import Parser.Parser
import Data.Data

import Debug.Trace

-- Data, Typeable, and Eq instances for the data types we we need to traverse
deriving instance (Eq a) => Eq (LS.Game a)
deriving instance Eq BoardDef
deriving instance Eq InputDef
deriving instance (Typeable a, Data a) => Data (LS.Game a)
deriving instance Data BoardDef
deriving instance Data InputDef
deriving instance (Typeable a, Data a) => Data (LS.ValDef a)
deriving instance Data Type
deriving instance Data Xtype
deriving instance Data LS.Signature
deriving instance (Typeable a, Data a) => Data (LS.Equation a)
deriving instance (Typeable a, Data a) => Data (LS.BoardEq a)
deriving instance (Typeable a, Data a) => Data (LS.Expr a)
deriving instance Data LS.Parlist
deriving instance Data LS.Pos
deriving instance Data LS.Op
deriving instance Data Btype
deriving instance Data Ftype


-- | Attributes we want to map to from their equivalent direct representations in the BoGL abstract syntax
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

-- | Defines subsumable attributes
instance Subsumable AttributeConcept where
  {-
  subsumes BinOp Equiv = True
  subsumes BinOp Div = True
  subsumes BinOp Mult = True
  subsumes BinOp Greater = True
  subsumes BinOp Less = True
  subsumes BinOp GreaterEqual = True
  subsumes BinOp LessEqual = True
  subsumes BinOp Sub = True
  subsumes BinOp Add = True
  -}
  subsumes _ _ = False

-- | Partial mapping of bogl syntactic categories (concepts) to refined ones (attribute concepts)
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
boglConceptMapping "BoardDef" = Just Bogl_Specifics.BoardDef
boglConceptMapping "InputDef" = Just Bogl_Specifics.InputDef
-- [ValDef SourcePos] ~ list of valdefs, (n)
boglConceptMapping _ = Nothing


-- | Parses a list of BoGL progs (from strings) into ASTs
parseBOGLPrograms :: [ConcreteProgram] -> [(String, LS.Game SourcePos)]
parseBOGLPrograms ls = fdown $ map (\(n,p) -> (n, parsePreludeAndGameText "" p "test")) ls

-- | Filter down to the programs that passeed parsing, reporting errors if any failed
fdown :: [(String,Either a (LS.Game SourcePos))] -> [(String,LS.Game SourcePos)]
fdown [] = []
fdown ((n,x):ls) = case x of
                      Left _  -> trace ("Failed to parse program '" ++ n ++ "'") fdown ls
                      Right g -> (n,g) : fdown ls
