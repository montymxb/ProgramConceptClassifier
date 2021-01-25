{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
--
-- BOGL_R1.hs
--
-- BoGL program directly written using the actual abstract syntax
--

module Programs.BOGL_R1 where

import Data.Data
import qualified Data.Set as S

-- BoGL Components
import Language.Syntax
import Language.Types
import Text.Parsec.Pos


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

-- Dummy position used to annotate tests quickly
dummyPos :: SourcePos
dummyPos = initialPos ""

-- Trying with a legitimate BoGL prog
-- Now analyze this program with 'cgraph'...then we can backstep to something simpler
{-
game TestGame

type Board = Array(3,3) of Int
type Input = Int

f1 : Int
f1 = 5

b1 : Board
b1!(x,y) = 0

f2 : Int -> Int
f2(x) = x + 10
-}
boglProg :: Game SourcePos
boglProg = (Game "TestGame" (BoardDef (3,3) (X Itype S.empty)) (InputDef (X Itype S.empty)) [
  -- val equation...f1 = 5
  (Val (Sig "f1" (Plain (X Itype S.empty))) (Veq "f1" (I 5)) dummyPos),
  -- board equation...b1!(1,y) = 0
  (BVal (Sig "b1" (Plain (X Board S.empty))) [(PosDef "b1" (ForAll "x") (ForAll "y") (I 0))] dummyPos),
  -- func equation...f2(x) = x + (10 - 5)
  (Val (Sig "f2" (Function (Ft (X Itype S.empty) (X Itype S.empty)))) (Feq "f2" (Pars ["x"]) (Binop Plus (Ref "x") (Binop Minus (I 10) (I 5)))) dummyPos)
  ])

boglProg2 :: Game SourcePos
boglProg2 = (Game "TestGame2" (BoardDef (3,3) (X Itype S.empty)) (InputDef (X Itype S.empty)) [
  -- func equation...double(x) = x * 2
  (Val (Sig "double" (Function (Ft (X Itype S.empty) (X Itype S.empty)))) (Feq "double" (Pars ["x"]) (Binop Times (Ref "x") (I 2))) dummyPos)
  ])
