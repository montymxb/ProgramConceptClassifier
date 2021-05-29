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
  | Plain_Types
  | Integer_Expressions
  | Names
  deriving (Eq,Show,Enum)

-- | Defines subsumable attributes
instance Subsumable AttributeConcept where
  {-
  -- subsumption relations that can be enabled as needed
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
boglConceptMapping "True" = Just Bool_Expressions
boglConceptMapping "B" = Just Bool_Expressions
boglConceptMapping "False" = Just Bool_Expressions
boglConceptMapping "ForAll" = Just For_All_Board_Positions
boglConceptMapping "Index" = Just Indexed_Board_Positions
boglConceptMapping "BoardEq SourcePos" = Just Board_Equations
boglConceptMapping "Get" = Just Get_Board_Value_Expression
boglConceptMapping "Tuple" = Just Tuples
boglConceptMapping "Game" = Just Game
boglConceptMapping "Val" = Just Values
boglConceptMapping "Type" = Just Types
boglConceptMapping "Feq" = Just Functions
boglConceptMapping "App" = Just Function_Applications
boglConceptMapping "Plus" = Just Addition
boglConceptMapping "Ref" = Just References
boglConceptMapping "I" = Just Integer_Expressions
boglConceptMapping "Op" = Just Binary_Operators
boglConceptMapping "Int" = Just Integer_Type
boglConceptMapping "[Char]" = Just Names
boglConceptMapping "BoardDef" = Just Board_Definitions
boglConceptMapping "InputDef" = Just Input_Definitions
boglConceptMapping _ = Nothing


-- | Parses a list of BoGL progs (from strings) into ASTs
parseBOGLPrograms :: [ConcreteProgram] -> [(String, Either ParseError (LS.Game SourcePos))]
parseBOGLPrograms ls = map (\(n,p) -> (n, parsePreludeAndGameText "" p n)) ls

-- | Get parsed programs
rightProgs :: Show a => [(String,Either a (LS.Game SourcePos))] -> [(String,LS.Game SourcePos)]
rightProgs [] = []
rightProgs ((n,x):ls) = case x of
                      Left l  -> rightProgs ls
                      Right g -> (n,g) : rightProgs ls

-- | Get unparsable programs
leftProgs :: Show a => [(String,Either a (LS.Game SourcePos))] -> [(String,a)]
leftProgs [] = []
leftProgs ((n,x):ls) = case x of
                      Left l  -> (n,l) : leftProgs ls
                      Right _ -> leftProgs ls
