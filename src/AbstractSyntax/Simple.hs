{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ConstrainedClassMethods #-}
--
-- Simple.hs
-- Super simple Abstract Syntax for a language we'll call a ConceptLanguage
-- Consists, solely of Statements that express themselves in terms of other statements.
--

module AbstractSyntax.Simple where

import ConceptGraph.Conceptual

import ConceptGraph.GraphToConceptGraph
import ConceptGraph.ConceptGraph
import GVSpec.GVSpec as GVSpec
import Data.Data

import ConceptGraph.Concept

data Simple = S3 A Simple
  | S2 B Simple
  | S1 C Simple
  | S0 [Statement]
  | End
  deriving (Show, Data, Eq)

-- smart constructors for structuring program parts
s1 :: Simple -> Simple
s1 = S1 (C 1)
s2 :: Simple -> Simple
s2 = S2 (B (C 1))
s3 :: Simple -> Simple
s3 = S3 (A (B (C 1)))

data A = A B deriving (Show, Data, Eq)
data B = B C deriving (Show, Data, Eq)
data C = C Int deriving (Show, Data, Eq)

data Statement = Statement0 Int
  | Statement1 Statement
  | Statement2 Statement Statement
  | Statement3 Statement Statement Statement
  deriving (Show, Data, Eq)


instance Conceptual Simple where
  concepts s@(S3 a b) = c2 s a b
  concepts s@(S2 a b) = c2 s a b
  concepts s@(S1 a b) = c2 s a b
  concepts s@(End) = cbase s
  concepts s@(S0 a) = c1 s a
  _edges s@(S3 a b) = e2 s a b
  _edges s@(S2 a b) = e2 s a b
  _edges s@(S1 a b) = e2 s a b
  _edges s@(End) = ebase s
  _edges s@(S0 a) = e1 s a ++ ebase s

instance Conceptual A where
  concepts s@(A a) = c1 s a
  _edges s@(A a) = e1 s a

instance Conceptual B where
  concepts s@(B a) = c1 s a
  _edges s@(B a) = e1 s a

instance Conceptual C where
  concepts s@(C a) = c1 s a
  _edges s@(C a) = e1 s a

instance Conceptual Statement where
  concepts s@(Statement0 a) = c1 s a
  concepts s@(Statement1 a) = c1 s a
  concepts s@(Statement2 a b) = c2 s a b
  concepts s@(Statement3 a b c) = c3 s a b c
  _edges s@(Statement0 a) = e1 s a
  _edges s@(Statement1 a) = e1 s a ++ ebase s
  _edges s@(Statement2 a b) = e2 s a b ++ ebase s
  _edges s@(Statement3 a b c) = e3 s a b c ++ ebase s

progs :: [Simple]
progs = [
  -- p0, simple program, no issues here
  s1 End,
  -- p1, all concepts in order
  s1 $ s2 $ s3 End,
  -- p2, jumps from C -> A, should be clear 'B' is missing for A to be understood
  s1 $ s3 End,
  -- p3, reverse order, should give us an error at the start, A -> B -> C, instead of C -> B -> A
  s3 $ s2 $ s1 End,
  -- p4, using a list of statements, in order
  S0 [Statement0 1,Statement1 (Statement0 2)],
  -- p5, using a list of statements, out of order
  S0 [Statement1 (Statement0 1), Statement0 2],
  -- p6, multiple lists of statements
  S0 [Statement3 (Statement1 (Statement0 1)) (Statement0 2) (Statement0 3)],
  -- p7
  S0 []]

graphSimpleProgs :: IO ()
graphSimpleProgs = _graphSimpleProgs (produceGraphs progs) 0

-- in order checks
o1 :: [Concept String]
o1 = map (\x -> (Concept x)) ["Concept","Simple","Int"]

knowns :: IO ()
knowns = _knowns 0 o1 (produceGraphs progs)

showDiffs :: [[Concept String]]
showDiffs = produceDiffs o1 (produceGraphs progs)
