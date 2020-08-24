{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts, DeriveAnyClass, DeriveGeneric #-}
--
-- BOGL_AST_TC.hs
-- WIP...work space until this is cleaned up
--
-- Produces a graph from a legitimate BoGL program
-- Experiments with doing this via the AST, and then stepping into the tree
-- produced by the type checker
--

module Implementations.BOGL_AST_TC where

import Grammar.Symbol
import Grammar.Rule
import Grammar.Grammar
import Grammar.GrammarToGraph
import ConceptGraph.GraphToConceptGraph
import ConceptGraph.ConceptGraph
import Query.Query
import GVSpec.GVSpec
import Data.Data
import Data.Generics
import Unsafe.Coerce
import qualified Data.Set as S

import Data.Generics.Uniplate.Data

-- bogl items
import Language.Syntax
import Language.Types


--
-- Going to just setup a conceptual typeclass, that works like show, but just gives details about the concepts, and their relationships
type EdgeName = String
type FromNode = String
type ToNode = String
type CNodes = [String]
type CEdges = [(EdgeName,FromNode,ToNode)]
class Conceptual a where
  cgraph :: (Typeable a, Data a, Eq a) => a -> (CNodes,CEdges)
  cgraph x = (concepts(x),_edges(x))

  concepts :: (Typeable a, Data a, Eq a) => a -> CNodes
  -- isAlgebraic
  concepts c = concatMap (\x -> case (isAlgType (dataTypeOf x)) of
                            True  -> [show (typeOf x)]
                            False -> []
                         ) $ universe c

  _edges :: (Typeable a, Data a, Eq a) => a -> CEdges
  -- default is not good enough, that's the problem...need to show the edge name as well!
  _edges x = [(" [label=\"" ++ (show (toConstr x)) ++ "\";]", show (typeOf x), "Concept")]

{-
instance Conceptual A1 where
  concepts a@(A1c x) = show (typeOf a) : concepts x
  _edges a@(A1c x) = (show (toConstr a), show (typeOf a), show (typeOf x)) : _edges x

instance Conceptual B1 where
  concepts b@(B1c x) = show (typeOf b) : concepts x
  _edges a@(B1c x) = (show (toConstr a), show (typeOf a), show (typeOf x)) : _edges x

--instance Show C1 where
--  show (C1c _) = "C1"

a1Val = (A1c (B1c (C1c 5)))
-}

-- Builds an edge from X -> Y
getEdge :: (Data a, Typeable a, Data b, Typeable b) =>  a -> b -> (EdgeName,FromNode,ToNode)
getEdge x y = (show (toConstr x), show (typeOf x), show (typeOf y))

instance Conceptual Int where
  concepts i = ["Int"]
  _edges i = []

instance (Data a, Eq a, Conceptual a) => Conceptual (Game a) where
  concepts g@(Game n b i v) = (show (typeOf g)) : (concepts b) ++ (concepts i) ++ (concatMap concepts v)
  _edges g@(Game n b i v) = [getEdge g b, getEdge g i, getEdge g v] ++ _edges b ++ _edges i ++ concatMap _edges v
instance Conceptual Signature where
  concepts s@(Sig n t) = (show (typeOf s)) : (concepts t)
  _edges s@(Sig n t) = [getEdge s t] ++ _edges t
instance Conceptual Parlist where
  concepts p@(Pars nl) = [(show (typeOf p))]
instance Conceptual BoardDef where
  concepts b@(BoardDef s p) = (show (typeOf b)) : (concepts p)
  _edges b@(BoardDef s p) = [getEdge b p] ++ _edges p
instance Conceptual InputDef where
  concepts i@(InputDef it) = (show (typeOf i)) : (concepts it)
  _edges i@(InputDef it) = [getEdge i it] ++ _edges it
instance (Data a, Eq a, Conceptual a) => Conceptual (ValDef a) where
  concepts v@(Val s eq v2) = (show (typeOf v)) : (concepts s) ++ (concepts eq) ++ (concepts v2)
  concepts v@(BVal s eqs v2) = (show (typeOf v)) : (concepts s) ++ (concatMap concepts eqs) ++ (concepts v2)
  _edges v@(Val s eq v2) = [getEdge v s, getEdge v eq, getEdge v v2] ++ _edges s ++ _edges eq ++ _edges v2
  _edges v@(BVal s eqs v2) = map (getEdge v) eqs ++ [getEdge v s, getEdge v v2] ++ _edges s ++ _edges v2 ++ concatMap _edges eqs
instance (Data a, Eq a, Conceptual a) => Conceptual (Equation a) where
  concepts e@(Veq n e2) = (show (typeOf e)) : (concepts e2)
  concepts e@(Feq n pl e2) = (show (typeOf e)) : (concepts pl) ++ (concepts e2)
  _edges e@(Veq n e2) = [getEdge e e2] ++ _edges e2
  _edges e@(Feq n pl e2) = [getEdge e pl, getEdge e e2] ++ _edges pl ++ _edges e2
instance (Data a, Eq a, Conceptual a) => Conceptual (BoardEq a) where
  concepts e@(PosDef n x y b) = (show (typeOf e)) : (concepts x) ++ (concepts y) ++ (concepts b)
  _edges e@(PosDef n x y b) = [getEdge e x, getEdge e y, getEdge e b] ++ _edges x ++ _edges y ++ _edges b
instance Conceptual Pos where
instance (Data a, Eq a, Conceptual a) => Conceptual (Expr a) where
  concepts t@(Tuple el) = (show (typeOf t)) : (concatMap concepts el)
  concepts t@(App n e) = (show (typeOf t)) : (concepts e)
  concepts t@(Binop o e1 e2) = (show (typeOf t)) : (concepts o) ++ (concepts e1) ++ (concepts e2)
  concepts t@(Let n e1 e2) = (show (typeOf t)) : (concepts e1) ++ (concepts e2)
  concepts t@(While e1 e2 nl e3) = (show (typeOf t)) : (concepts e1) ++ (concepts e2) ++ (concepts e3)
  concepts t@(If e1 e2 e3) = (show (typeOf t)) : (concepts e1) ++ (concepts e2) ++ (concepts e3)
  concepts t@(Annotation i e) = (show (typeOf t)) : (concepts i) ++ (concepts e)
  concepts t = [(show (typeOf t))]
  _edges t@(Tuple el) = (map (getEdge t) el) ++ (concatMap _edges el)
  _edges t@(App n e) = [getEdge t e] ++ _edges e
  _edges t@(Binop o e1 e2) = [getEdge t o, getEdge t e1, getEdge t e2] ++ _edges o ++ _edges e1 ++ _edges e2
  _edges t@(Let n e1 e2) = [getEdge t e1, getEdge t e2] ++ _edges e1 ++ _edges e2
  _edges t@(While e1 e2 nl e3) = [getEdge t e1, getEdge t e2, getEdge t e3] ++ _edges e1 ++ _edges e2 ++ _edges e3
  _edges t@(If e1 e2 e3) = [getEdge t e1, getEdge t e2, getEdge t e3] ++ _edges e1 ++ _edges e2 ++ _edges e3
  _edges t@(Annotation i e) = [getEdge t i, getEdge t e] ++ _edges i ++ _edges e
  _edges t = []
instance Conceptual Op where
instance Conceptual Btype where
instance Conceptual Xtype where
  concepts x@(X bt s) = (show (typeOf x)) : (concepts bt)
  concepts x@(Tup xtl) = (show (typeOf x)) : (concatMap concepts xtl)
  _edges x@(X bt s) = [getEdge x bt] ++ _edges bt
  _edges x@(Tup xtl) = map (getEdge x) xtl ++ concatMap _edges xtl
instance Conceptual Ftype where
  concepts f@(Ft x1 x2) = (show (typeOf f)) : (concepts x1) ++ (concepts x2)
  _edges f@(Ft x1 x2) = [getEdge f x1, getEdge f x2] ++ _edges x1 ++ _edges x2
instance Conceptual Type where
  concepts p@(Plain x1) = (show (typeOf p)) : (concepts x1)
  concepts p@(Function f1) = (show (typeOf p)) : (concepts f1)
  _edges p@(Plain x1) = [getEdge p x1] ++ _edges x1
  _edges p@(Function f1) = [getEdge p f1] ++ _edges f1


-- Trying with a legitimate BoGL prog
-- Now analyze this program with 'cgraph'...then we can backstep to something simpler
boglProg :: Game Int
boglProg = (Game "TestGame" (BoardDef (3,3) (X Itype S.empty)) (InputDef (X Itype S.empty)) [
  -- val equation...f1 = 5
  (Val (Sig "f1" (Plain (X Itype S.empty))) (Veq "f1" (I 5)) 11),
  -- board equation...b1!(1,y) = 0
  (BVal (Sig "b1" (Plain (X Board S.empty))) [(PosDef "b1" (Index 1) (ForAll "y") (I 0))] 22),
  -- func equation...f2(x) = x + 10
  (Val (Sig "f2" (Function (Ft (X Itype S.empty) (X Itype S.empty)))) (Feq "f2" (Pars ["x"]) (Binop Plus (Ref "x") (I 10))) 33)
  ])

-- concept graph in pure BoGL
-- GVSpec.writeGVSpec "astgraph" boglGraph
boglGraph :: ConceptGraph String String
boglGraph = graph_to_concept_graph $ cgraph boglProg
