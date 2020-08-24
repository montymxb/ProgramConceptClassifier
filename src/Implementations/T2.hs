{-# LANGUAGE DeriveDataTypeable #-}
module Implementations.T2 where

import Data.Generics.Uniplate.Data    -- uniplate
import Data.Data

data Expr2 = Pre Expr2 | SVar2 String
  deriving (Data,Show,Typeable)

data Expr  =  Add  Expr  Expr  | Val  Int
           |  Sub  Expr  Expr  | Var  String
           |  Jmp Expr2
           |  Mul  Expr  Expr  | Neg  Expr
           |  Div  Expr  Expr
           deriving (Data,Show,Typeable)

e1 = (Add (Neg (Sub (Var "x") (Var "y"))) (Jmp (Pre (SVar2 "zzz"))))

data Test1 = Test1 {name :: String, name2 :: String}
  deriving (Data,Typeable,Show)

qe1 = Test1 "whoa" "hey"
fe1 = constrFields(toConstr(qe1))

data Test2 = Test2 String String
  deriving (Data,Typeable,Show)

qe2 = Test2 "whoa2" "hey2"
fe2 = constrFields(toConstr(qe2))


-- okay, so we can use universe how...
var2s :: Expr2 -> [String]
var2s x = map (\y -> show (typeOf y)) (universe x)

-- use uniplate to collect all variables in this
var1s :: Expr -> [String]
var1s x = concatMap (\x -> case x of
                      (Jmp q) -> var2s q
                      q       -> [show (typeOf q)]) (universe x)

www = universe e1
