{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ConstrainedClassMethods #-}
--
-- ManualConcepts.hs
-- Nov. 22nd, 2020
-- Manual Relating of Concepts pulled out by Hand from BoGL programs (based on Martin's email)
--
module ConceptGraph.ManualConcepts where

import Data.Data

{-
# Possible Expr Order (by Hand)

1. Literal (Int,Bool,Symbol)
2. BinOp (+,-,*,/)
3. Parentheses (Order of Operations)
4. If-Then-Else
5. BinOp (==,<=,>=,<,>,/=)
6. Let
7. Reference to a name
8. 2-Tuple
9. 3-Tuple
10. N-ary Tuple
11. Nested Tuples
12. Function Application
13. While
-}

-- There was also this question about visualizing information for programs, for Traces, but it's been difficult to do this...hmmm.hmmm.hmmm
-- It would be great if you could request the environment at any point, and get back what's available to work with...a little query system for the language...

data Dependency a = Single a
  | Or [Dependency a]
  | And [Dependency a]
  deriving (Show,Data)

data Concept = Game
  | Type
  | ReturningType
  | AcceptingType
  | TypeDef
  | TupleType
  | ExtendedType
  | BoardType
  | Board
  | Array
  | Dimensions
  | Content
  | Input
  | InputType
  | Definition    -- either Val or Func def
  | ValueDef      -- value def
  | ValueSignature --
  | ValueEquation
  | Expr --
  | IntType --
  | LiteralInt --
  | BoolType
  | LiteralBool --
  | SymbolType
  | LiteralSymbol --
  | BinOp -- done
  | BinOpArithmetic -- done
  | Add -- done
  | Sub -- done
  | Mul -- done
  | Div -- done
  -- | Parentheses_OrderOfOperations, where to put this, can expand in 4 different ways (one of which is redundant)
  | IfThenElse --
  | Condition --
  | BinOpConditional -- done
  | Eq -- done
  | NotEq --
  | LessThan --
  | GreaterThan --
  | LessThanEqualTo --
  | GreaterThanEqualTo --
  | Let --
  | Name --
  | Reference -- reference to a bound name
  | Tuple --
  | FunctionDef
  | FunctionSignature
  | FunctionEquation  -- equation has Name, param, expr
  | Parameter   -- for a func eq
  | FunctionApplication
  | While       -- done
  | BoardDef    -- done
  | BoardSignature -- done
  | BoardEquation -- done
  | Position
  | PositionAll
  | PositionColumn
  | PositionRow
  | PositionExact
  | Get
  deriving (Eq,Show,Data)

(-->) :: c -> d -> (c,d)
c --> d = (c,d)

orDep :: [a] -> Dependency a
orDep = Or . map Single

andDep :: [a] -> Dependency a
andDep = And . map Single

-- then graph this out...
conceptDepGraph :: [(Concept,Dependency Concept)]
conceptDepGraph = [
       -- game
       Game             --> And [Single BoardType, Single InputType, Single Definition],

       -- types
       Type             --> orDep [IntType,BoolType,SymbolType,TupleType,ExtendedType,BoardType,InputType,Content],
       ReturningType    --> andDep [Type],
       AcceptingType    --> andDep [Type],

       -- various defs
       Definition       --> orDep [ValueDef,FunctionDef,BoardDef,TypeDef],

       -- base equation & signature variants
       -- removed these in favor of ValueDef,Functiondef,BoardDef, and TypeDef as Or deps of Definition above ^^^
       --Equation         --> orDep [ValueEquation,FunctionEquation,BoardEquation],
       --Signature        --> orDep [ValueSignature,FunctionSignature],

       -- type def
       TypeDef          --> andDep [Name,Type],

       -- board
       BoardType        --> andDep [Board,Array,Type],
       Array            --> andDep [Dimensions],

       -- input
       InputType        --> andDep [Input,Type],

       -- value
       ValueDef         --> andDep [Name,ValueSignature,ValueEquation],
       ValueSignature   --> andDep [Name,ReturningType],
       ValueEquation    --> andDep [Name,Expr],

       -- function
       FunctionDef      --> andDep [Name,FunctionSignature,FunctionEquation],
       FunctionSignature--> andDep [Name,AcceptingType,ReturningType],
       FunctionEquation --> andDep [Name,Parameter,Expr],

       -- parameter is for function equations
       Parameter        --> Or [
                                  andDep [Name],       -- parameter can be a single name
                                  andDep [Tuple,Name]],-- or a tuple of names....before this was Expr, were you thinking in terms of bindings directly, like let?

       -- board
       BoardDef         --> andDep [Name,BoardSignature,BoardEquation],
       BoardSignature   --> andDep [Name,Board],
       BoardEquation    --> Or [
                                  andDep [Name,PositionAll,Expr],   -- I thought to represent this differently, as there are only 4 expressible cases
                                  andDep [Name,PositionColumn,Expr],
                                  andDep [Name,PositionRow,Expr],
                                  andDep [Name,PositionExact,Expr]],
       PositionAll      --> andDep [Name,Name],
       PositionColumn   --> andDep [Expr,Name],
       PositionRow      --> andDep [Name,Expr],
       PositionExact    --> andDep [Expr,Expr],

       -- expr
       Expr             --> orDep [LiteralInt,LiteralBool,LiteralSymbol,BinOp,IfThenElse,Let,Reference,Tuple,FunctionApplication,While,Get],
       BinOp            --> orDep [BinOpArithmetic,BinOpConditional],
       BinOpArithmetic  --> orDep [Add,Sub,Mul,Div],
       BinOpConditional --> orDep [LessThan,GreaterThan,Eq,NotEq,LessThanEqualTo,GreaterThanEqualTo],
       IfThenElse       --> andDep [Condition,Expr],
       Condition        --> andDep [Expr],
       Let              --> andDep [Name,Expr], -- better way to express a change to the environment at this point
       Reference        --> andDep [Name],
       FunctionApplication --> andDep [Name,Tuple],
       While            --> andDep [Condition,Expr],
       Get              --> andDep [Name,Position],
       Position         --> andDep [Tuple]
      ]

-- easiest way to ground would be to take a concept lattice from a BoGL program, and directly ground to the concepts by name, rather than analyzing the tree
grounding :: [(String,String)]
grounding = undefined
