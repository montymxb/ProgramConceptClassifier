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

-- bogl items
import Language.Syntax


--
-- TODO Continue with this general approach to make this work with the concept graph
-- Once that looks good, go ahead and hook it into BoGL itself
--

-- Going to just setup a conceptual typeclass, that works like show, but just gives details about the concepts, and their relationships
type EdgeName = String
type FromNode = String
type ToNode = String
type CNodes = [String]
type CEdges = [(EdgeName,FromNode,ToNode)]
class Conceptual a where
  cgraph :: (Typeable a, Data a) => a -> (CNodes,CEdges)
  cgraph(x) = (concepts(x),_edges(x))

  concepts :: (Typeable a, Data a) => a -> CNodes
  concepts(x) = [show (typeOf x)]

  _edges :: (Typeable a, Data a) => a -> CEdges
  _edges(x) = [(show (toConstr x), show (typeOf x), "")]

-- For testing, that's it
data C1 = C1c Int
  deriving(Data,Typeable,Conceptual)
data B1 = B1c C1
  deriving(Data,Typeable)
data A1 = A1c B1
  deriving(Data,Typeable)


instance Conceptual A1 where
  concepts a@(A1c x) = show (typeOf a) : concepts x
  _edges a@(A1c x) = (show (toConstr a), show (typeOf a), show (typeOf x)) : _edges x

instance Conceptual B1 where
  concepts b@(B1c x) = show (typeOf b) : concepts x
  _edges a@(B1c x) = (show (toConstr a), show (typeOf a), show (typeOf x)) : _edges x

--instance Show C1 where
--  show (C1c _) = "C1"

a1Val = (A1c (B1c (C1c 5)))




-- Trying with a legitimate BoGL prog
--boglProg ::




-- A version of Data.Generics.listify which doesn't recurse into sublists of type [b]
listifyWholeLists :: (Typeable b, Data b, Show b) => ([b] -> Bool) -> GenericQ [[b]]
listifyWholeLists blp = flip (synthesize id (.) (mkQ id (\bl _ -> if blp bl then (bl:) else id))) []

--

{- | C tags the type that is actually parameterized, so to avoid touching the
Int when a ~ Int:

> data T a = T Int a

by changing the type (not representation) to:

> x :: T Int (C Int)
-}
newtype C a = C a deriving (Data,Typeable)

-- Generic Fmap over all nodes in a data type
-- BOGL.fmapData show [BOGL.grammar :: Grammar]
-- BOGL.fmapData printFromConstructor

-- for BoGL this will work but ONLY on a program that actually uses it's values...
-- instead...we want one for the entire possible prog, and then one for the actual prog
-- i.e. one to compare against, and one to use to evaluate a program
--
-- as the function to fmapData...apply itself in such a way as to print and apply itself to all sub values

fmapData :: forall t a b. (Typeable a, Data (t (C a)), Data (t a)) => (a -> b) -> t a -> t b
fmapData f input = uc . everywhere (mkT $ \(x::C a) -> uc (f (uc x))) $ (uc input :: t (C a))
                   where uc = unsafeCoerce


-- TODO, working on printing out the entire AST from a consturctor alone...
fapp :: (Data a, Typeable a, Show a) => a -> [String]
fapp x =  let typ = dataTypeOf (x) in
          --let constrs = dataTypeConstrs typ in
          (show typ) : [(show x)]--concat (fmapData fapp [x])
                         --map show constrs ++ [(show $ map constrFields constrs)]

-- .. try out on the 'Grammar' datatype
-- TODO remove
testthis :: (Typeable a, Data a) => a -> [String]
testthis item = let constrs = (gmapQ (\d -> toConstr d) item) in
                map show constrs -- ++ concatMap (\(D d) -> testthis d) constrs -- ++ (concat (map testthis constrs))
