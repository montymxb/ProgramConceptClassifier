{-# LANGUAGE DeriveDataTypeable #-}

module AbstractSyntax.BOGL where

--
-- BoGL Abstract Syntax representation
--

import Data.Data

-- lower case starting name
-- [a-z][a-zA-Z0-9_]
data LName = LName String
  deriving (Show,Data,Eq)

-- upper case starting name
-- [A-Z][a-zA-Z0-9_]
data UName = UName String
  deriving (Show,Data,Eq)

-- base type
data BType = BInt Int
  | BBoard Board
  | BUName UName
  | BTuple [BType] -- k >= 2
  deriving (Show,Data,Eq)

-- function type
data FType = FType BType BType
  deriving (Show,Data,Eq)

-- enumerated type
data EType = EType [UName] -- k >= 1
  deriving (Show,Data,Eq)

-- extension type
data XType = X_EType EType
  | X_ExtEType BType EType
  | X_ExtName BType UName
  deriving (Show,Data,Eq)

-- game type, all programs are this
data Game = Game UName [TypeOrValDef] Board Input [ValDef]
  deriving (Show,Data,Eq)

-- type or valdef
data TypeOrValDef = TOV_TypeAssign TypeAssign
  | TOV_ValDef ValDef
  deriving (Show,Data,Eq)

-- board type assignment
data Board = Board Int Int BType
  deriving (Show,Data,Eq)

-- input type assignment
data Input = Input BType
  deriving (Show,Data,Eq)

-- type declaration or synonym
data TypeAssign = TypeAssignXType UName XType
  | TypeAssignBType UName BType
  deriving (Show,Data,Eq)

-- value definition
data ValDef = ValDef Signature Equation
  deriving (Show,Data,Eq)

-- type signature
data Signature = BSig LName BType
  | FSig LName FType
  deriving (Show,Data,Eq)

-- equation
data Equation = ValEquation LName Expr -- val equation
  | FuncEquation LName [LName] Expr    -- function equation
  | BoardEquation LName Pos Pos Expr   -- board equation
  deriving (Show,Data,Eq)

-- position, either a name or a direct value
data Pos = NamePos LName
  | IntPos Int
  deriving (Show,Data,Eq)

-- expressions
data Expr = IVal Int
  | SVal UName            -- symbol
  | Ref LName             -- reference
  | Tup [Expr]            -- tuple
  | App LName [Expr]      -- function application
  | BinOp Expr BinOp Expr -- infix binary application
  | Let LName Expr Expr    -- let expression
  | Cond Expr Expr Expr   -- conditional branch
  | While Expr Expr       -- while loop
  deriving (Show,Data,Eq)

data BinOp = Plus
  | Minus
  | Times
  | Div
  | Less
  | LessEq
  | Eq
  | GreaterEq
  | Greater
  | NotEq
  | Get
  deriving (Show,Data,Eq)
