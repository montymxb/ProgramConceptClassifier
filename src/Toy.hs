-- Toy language for analyzing
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Toy where

import Data.Data
import General

data Const = Tru | Fls
  deriving (Show,Data,Eq)
data Op = And | Or
  deriving (Show,Data,Eq)
data UOp = Neg
  deriving (Show,Data,Eq)

data L = L1 Const | L2 Op L L | L3 UOp L | IfThenElse L L L
  deriving (Show,Data,Eq)

p1 = ("P1", L1 Tru)
p2 = ("P2", L2 Or (L1 Fls) (L1 Tru))
p3 = ("P3", L3 Neg (L1 Tru))
p4 = ("P4", L3 Neg (L2 And (L1 Tru) (L1 Tru)))
p5 = ("P5", IfThenElse (L1 Fls) (L1 Tru) (L1 Fls))
p6 = ("P6", IfThenElse (L1 Tru) (L2 Or (L1 Fls) (L1 Tru)) (L1 Fls))
p7 = ("P7", IfThenElse (L2 And (L1 Tru) (L1 Fls)) (L3 Neg (L1 Tru)) (L3 Neg (L1 Fls)))

data ToyConcept = Const | BinaryOp | UnaryOp | Program | Branching

instance Show ToyConcept where
  show Const = "Constant"
  show BinaryOp = "Binary Op"
  show UnaryOp = "Unary Op"
  show Program = "Program"
  show Branching = "Branching"

instance Subsumable ToyConcept where
  subsumes _ _ = False

toyConceptMapping :: String -> Maybe ToyConcept
toyConceptMapping "L" = Just Program
toyConceptMapping "Const" = Just Const
toyConceptMapping "Op" = Just BinaryOp
toyConceptMapping "UOp" = Just UnaryOp
toyConceptMapping "IfThenElse" = Just Branching
toyConceptMapping _ = Nothing
