{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Bogl_Specifics where

---
--- BoGL Specific Stuff
---

import qualified Language.Syntax as LS
import Language.Types
import Text.Parsec.Pos
import Text.Parsec.Error
import General

import Parser.Parser
import Data.Data

-- | Orphan Data, Typeable (and Eq for a few cases) instances for the BoGL AST data types
-- These are critical to allow generic mapping of an otherwise opaque structure
-- Fixed approaches can be devised for an encoding on a case by case basis,
-- but they are inflexible, time-consuming to write, and potentially error prone
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
data AttributeConcept = Relational_Inequality
  | Greater_Than_or_Equal_To
  | Less_Than_or_Equal_To
  | Symbol_Expressions
  | Relational_Equality
  | Division
  | Let_Expressions
  | Multiplication
  | Greater_Than
  | Subtraction
  | Less_Than
  | While_Loops
  | Branching
  | Tuples
  | Bool_Expressions
  | For_All_Board_Positions
  | Indexed_Board_Positions
  | Board_Equations
  | Value_Equations
  | Get_Board_Value_Expression
  | Game
  | Values
  | Functions
  | Types
  | Function_Applications
  | Binary_Operators
  | Addition
  | References
  | Integer_Type
  | Board_Definitions
  | Input_Definitions
  | Enumeration_Types
  | Tuple_Types
  | Bool_Type
  -- | True_Value
  -- | False_Value
  | Plain_Types
  | Integer_Expressions
  | Names
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
boglConceptMapping "Top" = Just Enumeration_Types
boglConceptMapping "NotEquiv" = Just Relational_Inequality
boglConceptMapping "Geq" = Just Greater_Than_or_Equal_To
boglConceptMapping "Leq" = Just Less_Than_or_Equal_To
boglConceptMapping "S" = Just Symbol_Expressions
boglConceptMapping "Equiv" = Just Relational_Equality
boglConceptMapping "Div" = Just Division
boglConceptMapping "Let" = Just Let_Expressions
boglConceptMapping "Times" = Just Multiplication
boglConceptMapping "Greater" = Just Greater_Than
boglConceptMapping "Minus" = Just Subtraction
boglConceptMapping "Less" = Just Less_Than
boglConceptMapping "While" = Just While_Loops
boglConceptMapping "If" = Just Branching
boglConceptMapping "Tup" = Just Tuple_Types
boglConceptMapping "Booltype" = Just Bool_Type
-- [Xtype] ~ a list of Xtypes (n)
boglConceptMapping "True" = Just Bool_Expressions
-- B ~ constructor for expression that evaluates to a Boolean, should probably have this? (n)
boglConceptMapping "B" = Just Bool_Expressions
boglConceptMapping "False" = Just Bool_Expressions
-- Bool ~ not needed, as we will always have 'B' above when this is here too (the type of the value in B)
boglConceptMapping "ForAll" = Just For_All_Board_Positions
-- BVal ~ a Board Value (n), doesn't add anything new
-- Board ~ subsumed by BoardEq
-- PosDef ~ instance of a Board Equation... (n)
boglConceptMapping "Index" = Just Indexed_Board_Positions
-- Pos ~ no help (n)
boglConceptMapping "BoardEq SourcePos" = Just Board_Equations
-- [BoardEq SourcePos] ~ array of board equations, which are the same as array of PosDef (n)
-- TODO removed for paper analysis
--boglConceptMapping "Plain" = Just Plain_Types
-- TODO removed for paper analysis
--boglConceptMapping "Veq" = Just Value_Equations
boglConceptMapping "Get" = Just Get_Board_Value_Expression
boglConceptMapping "Tuple" = Just Tuples
-- [Expr SourcePos] ~ List of expressions (n)
-- Game SourcePos ~ Annotated Game (n)
boglConceptMapping "Game" = Just Game
-- (,) ~ Tuple constructor (n)
-- (Int,Int) ~ Tuple of Ints constructor (n)
boglConceptMapping "Val" = Just Values
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
boglConceptMapping "Type" = Just Types
boglConceptMapping "Feq" = Just Functions
-- Pars ~ parameters, nothing added
-- [[Char]] ~ List of names, probably for symbols & or other thing (n)
boglConceptMapping "App" = Just Function_Applications
-- BinOp ~ does not add anything 'Op' already does
boglConceptMapping "Plus" = Just Addition
boglConceptMapping "Ref" = Just References
-- Annotation ~ used for annotating for parsing (n)
boglConceptMapping "I" = Just Integer_Expressions
boglConceptMapping "Op" = Just Binary_Operators
-- Parlist ~ list of names to bind to funtion equation parameters (n)
-- Expr SourcePos ~ nothing lost here
-- (:) ~ cons (n)
-- Char ~ single char (n)
boglConceptMapping "Int" = Just Integer_Type
-- Signature ~ general signature (n)
-- Equation SourcePos ~ general equation (n)
-- SourcePos ~ annotation stuff (n)
-- [] ~ empty list (n)
-- ValDef SourcePos ~ value definition (n)
boglConceptMapping "[Char]" = Just Names
boglConceptMapping "BoardDef" = Just Board_Definitions
boglConceptMapping "InputDef" = Just Input_Definitions
-- [ValDef SourcePos] ~ list of valdefs, (n)
boglConceptMapping _ = Nothing


-- | Parses a list of BoGL progs (from strings) into ASTs
parseBOGLPrograms :: [ConcreteProgram] -> [(String, Either ParseError (LS.Game SourcePos))]
parseBOGLPrograms ls = map (\(n,p) -> (n, parsePreludeAndGameText "" p n)) ls

-- | Get parsed programs
rightProgs :: Show a => [(String,Either a (LS.Game SourcePos))] -> [(String,LS.Game SourcePos)]
rightProgs [] = []
rightProgs ((n,x):ls) = case x of
                      Left _  -> rightProgs ls
                      Right g -> (n,g) : rightProgs ls

-- | Get unparsable programs
leftProgs :: Show a => [(String,Either a (LS.Game SourcePos))] -> [(String,a)]
leftProgs [] = []
leftProgs ((n,x):ls) = case x of
                      Left l  -> (n,l) : leftProgs ls
                      Right _ -> leftProgs ls
