{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
--
-- BOGL_AST_TC.hs
-- WIP...work space until this is cleaned up
--
-- Produces a graph from a legitimate BoGL program
-- Experiments with doing this via the AST, and then stepping into the tree
-- produced by the type checker
--

module Implementations.BOGL_AST_TC where

import Grammar.Grammar
import ConceptGraph.GraphToConceptGraph
import ConceptGraph.ConceptGraph
import Query.Query
import GVSpec.GVSpec
import Data.Data
import Data.Generics
import Unsafe.Coerce
import qualified Data.Set as S

import Data.Generics.Uniplate.Data

-- BoGL Components
import Language.Syntax
import Language.Types
import Text.Parsec.Pos
import qualified Typechecker.Typechecker as TC


--
-- Going to just setup a conceptual typeclass, that works like show, but just gives details about the concepts, and their relationships
--
type EdgeName = String
type FromNode = String
type ToNode = String
type CNodes = [String]
type CEdges = [(EdgeName,FromNode,ToNode)]
class Conceptual a where
  cgraph :: (Typeable a, Data a, Eq a) => a -> (CNodes,CEdges)
  cgraph x = (concepts(x),_edges(x))

  concepts :: (Typeable a, Data a, Eq a) => a -> CNodes
  -- is unused...
  concepts c = concatMap (\x -> case (isAlgType (dataTypeOf x)) of
                            True  -> [show (typeOf x)]
                            False -> []
                         ) $ universe c

  _edges :: (Typeable a, Data a, Eq a) => a -> CEdges
  -- default is acceptable for now, everything ties into a 'concept'
  _edges x = [("",show (typeOf x), show (toConstr x)),("",show (toConstr x), "Concept")] --[(show (toConstr x), show (typeOf x), "Concept")]

-- Self edge
selfEdge :: (Data a, Typeable a) => a -> (EdgeName,FromNode,ToNode)
selfEdge x = ("",show (typeOf x), show (toConstr x))

-- Builds an edge from X -> Y
getEdge :: (Data a, Typeable a, Data b, Typeable b) =>  a -> b -> (EdgeName,FromNode,ToNode)
getEdge x y = ("", show (toConstr x), show (typeOf y)) -- (show (toConstr x), show (typeOf x), show (typeOf y))

instance Conceptual Int where
  concepts x = ["Int","Concept"]
  _edges x = [("","Int","Concept")]

instance Conceptual Double where
  concepts x = ["Double","Concept"]
  _edges x = [("", "Double", "Concept")]

instance Conceptual Float where
  concepts x = ["Float","Concept"]
  _edges x = [("", "Float", "Concept")]

instance Conceptual Word where
  concepts x = ["Word","Concept"]
  _edges x = [("", "Word", "Concept")]

instance Conceptual Bool where
  concepts x = ["Bool","Concept"]
  _edges x = [("", "Bool", "Concept")]

instance Conceptual Char where
  concepts x = ["Char","Concept"]
  _edges x = [("", "Char", "Concept")]--[("prim", show (typeOf x), "Concept")]

-- TODO some issues with [Char] being mapped to essentially nothing
-- should go [Char] -> Char -> Concept
-- but instead goes [Char] -> ... nothing
-- and Char is stranded, ... -> Char -> ..., needs to follow properly
-- partly stranded from the name of a game, which is also a string...look at that
-- Problem is that '[Char]' doesn't appear on the LHS of any rule for a list of characters
instance (Data a, Eq a, Conceptual a) => Conceptual [a] where
  concepts x = (show (typeOf x)) : (show (toConstr x)) : concatMap concepts x
  _edges []       = [] -- nothing...
  -- we want to map from list constructor to 'i' for every item in the list
  _edges x@(i:ls) = ("", show (typeOf x), (show (toConstr x))) : ("", (show (toConstr x)), "Concept") : map (\z -> ("", show (typeOf x), show (typeOf z))) (i:ls) ++ concatMap _edges x
    --map (\z -> (show (toConstr x), show (typeOf x), show (typeOf z))) (i:ls) ++ concatMap _edges x

instance (Data a, Eq a, Conceptual a) => Conceptual (Game a) where
  concepts g@(Game n b i v) = (show (typeOf g)) : "Game" : (concepts n) ++ (concepts b) ++ (concepts i) ++ (concepts v)
  _edges g@(Game n b i v) = [selfEdge g, getEdge g n, getEdge g b, getEdge g i, getEdge g v] ++ _edges n ++ _edges b ++ _edges i ++ (_edges v)

instance Conceptual Signature where
  concepts s@(Sig n t) = (show (typeOf s)) : "Sig" : (concepts t) ++ (concepts n)
  _edges s@(Sig n t) = [selfEdge s, getEdge s t, getEdge s n] ++ _edges t

instance Conceptual Parlist where
  concepts p@(Pars nl) = [show (typeOf p),"Pars"]

instance Conceptual BoardDef where
  concepts b@(BoardDef s p) = (show (typeOf b)) : "BoardDef" : (concepts p)
  _edges b@(BoardDef s p) = [getEdge b p] ++ _edges p -- NO self edge, name is the same

instance Conceptual InputDef where
  concepts i@(InputDef it) = (show (typeOf i)) : "InputDef" : (concepts it)
  _edges i@(InputDef it) = [getEdge i it] ++ _edges it -- NO self edge, name is the same

instance (Data a, Eq a, Conceptual a) => Conceptual (ValDef a) where
  concepts v@(Val s eq v2) = (show (typeOf v)) : "Val" : (concepts s) ++ (concepts eq) ++ (concepts v2)
  concepts v@(BVal s eqs v2) = (show (typeOf v)) : "BVal" : (concepts s) ++ (concatMap concepts eqs) ++ (concepts v2)
  _edges v@(Val s eq v2) = [selfEdge v, getEdge v s, getEdge v eq, getEdge v v2] ++ _edges s ++ _edges eq ++ _edges v2
  _edges v@(BVal s eqs v2) = selfEdge v : map (getEdge v) eqs ++ [getEdge v s, getEdge v v2] ++ _edges s ++ _edges v2 ++ concatMap _edges eqs

instance (Data a, Eq a, Conceptual a) => Conceptual (Equation a) where
  concepts e@(Veq n e2) = (show (typeOf e)) : "Veq" : (concepts e2)
  concepts e@(Feq n pl e2) = (show (typeOf e)) : "Feq" : (concepts pl) ++ (concepts e2)
  _edges e@(Veq n e2) = [selfEdge e, getEdge e e2] ++ _edges e2
  _edges e@(Feq n pl e2) = [selfEdge e, getEdge e pl, getEdge e e2] ++ _edges pl ++ _edges e2

instance (Data a, Eq a, Conceptual a) => Conceptual (BoardEq a) where
  concepts e@(PosDef n x y b) = (show (typeOf e)) : "PosDef" : (concepts x) ++ (concepts y) ++ (concepts b)
  _edges e@(PosDef n x y b) = [selfEdge e, getEdge e x, getEdge e y, getEdge e b] ++ _edges x ++ _edges y ++ _edges b

instance Conceptual Pos where
  concepts e@(Index i) = [show (typeOf e),"Index"]
  concepts e@(ForAll n) = [show (typeOf e),"ForAll"]
  _edges e@(Index i) = [selfEdge e, getEdge e i]
  _edges e@(ForAll n) = [selfEdge e, getEdge e n]

instance (Data a, Eq a, Conceptual a) => Conceptual (Expr a) where
  --concepts t@(I i) = (show (typeOf t))
  --concepts t@(B i) = (show (typeOf t))
  --concepts t@(Ref i) = (show (typeOf t))
  concepts t@(Tuple el) = (show (typeOf t)) : "Tuple" : (concatMap concepts el)
  concepts t@(App n e) = (show (typeOf t)) : "App" : (concepts e)
  concepts t@(Binop o e1 e2) = (show (typeOf t)) : "Binop" : (concepts o) ++ (concepts e1) ++ (concepts e2)
  concepts t@(Let n e1 e2) = (show (typeOf t)) : "Let" : (concepts e1) ++ (concepts e2)
  concepts t@(While e1 e2 nl e3) = (show (typeOf t)) : "While" : (concepts e1) ++ (concepts e2) ++ (concepts e3)
  concepts t@(If e1 e2 e3) = (show (typeOf t)) : "If" : (concepts e1) ++ (concepts e2) ++ (concepts e3)
  concepts t@(Annotation i e) = (show (typeOf t)) : "Annotation" : (concepts i) ++ (concepts e)
  --concepts t@(HE n) = (show (typeOf t))
  concepts t = [show (typeOf t), show (toConstr t)]

  _edges t@(Tuple el) = selfEdge t : (map (getEdge t) el) ++ (concatMap _edges el)
  _edges t@(App n e) = [selfEdge t, getEdge t e] ++ _edges e
  _edges t@(Binop o e1 e2) = [selfEdge t, getEdge t o, getEdge t e1, getEdge t e2] ++ _edges o ++ _edges e1 ++ _edges e2
  _edges t@(Let n e1 e2) = [selfEdge t, getEdge t e1, getEdge t e2] ++ _edges e1 ++ _edges e2
  _edges t@(While e1 e2 nl e3) = [selfEdge t, getEdge t e1, getEdge t e2, getEdge t e3] ++ _edges e1 ++ _edges e2 ++ _edges e3
  _edges t@(If e1 e2 e3) = [selfEdge t, getEdge t e1, getEdge t e2, getEdge t e3] ++ _edges e1 ++ _edges e2 ++ _edges e3
  _edges t@(Annotation i e) = [selfEdge t, getEdge t i, getEdge t e] ++ _edges i ++ _edges e
  _edges t = [selfEdge t, ("", show (toConstr t), "Concept")]

instance Conceptual Op where
instance Conceptual Btype where
instance Conceptual Xtype where
  concepts x@(X bt s) = (show (typeOf x)) : "X" : (concepts bt)
  concepts x@(Tup xtl) = (show (typeOf x)) : "Tup" : (concatMap concepts xtl)
  _edges x@(X bt s) = [selfEdge x, getEdge x bt] ++ _edges bt
  _edges x@(Tup xtl) = selfEdge x : map (getEdge x) xtl ++ concatMap _edges xtl

instance Conceptual Ftype where
  concepts f@(Ft x1 x2) = (show (typeOf f)) : "Ft" : (concepts x1) ++ (concepts x2)
  _edges f@(Ft x1 x2) = [selfEdge f, getEdge f x1, getEdge f x2] ++ _edges x1 ++ _edges x2

instance Conceptual Type where
  concepts p@(Plain x1) = (show (typeOf p)) : "Plain" : (concepts x1)
  concepts p@(Function f1) = (show (typeOf p)) : "Function" : (concepts f1)
  _edges p@(Plain x1) = [selfEdge p, getEdge p x1] ++ _edges x1
  _edges p@(Function f1) = [selfEdge p, getEdge p f1] ++ _edges f1

instance Conceptual SourcePos where
  concepts sp = []
  _edges sp = []


--
-- Allows automatically deriving typeclass instances for the pre-existing data types we want to work with
--
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
  -- func equation...f2(x) = x + 10
  (Val (Sig "f2" (Function (Ft (X Itype S.empty) (X Itype S.empty)))) (Feq "f2" (Pars ["x"]) (Binop Plus (Ref "x") (Binop Minus (I 10) (I 5)))) dummyPos)
  ])



-- concept graph in pure BoGL
-- GVSpec.writeGVSpec "astgraph" boglGraph
boglGraph :: ConceptGraph String String
boglGraph = graph_to_concept_graph $ cgraph boglProg
