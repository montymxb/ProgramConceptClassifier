{-# LANGUAGE DeriveDataTypeable #-}
--
-- Concept Graph describing the relationships amongst
-- concepts in a given context, with relationships established via
-- a grammar's rule set
--

module ConceptGraph.ConceptGraph (ConceptGraph(ConceptGraph),ConceptLattice,concepts,edges) where

import ConceptGraph.Concept
import ConceptGraph.ConceptDependency
import Data.Data

-- | ConceptGraph type, represents a graph with a list of concepts (vertices) and depedencies amongst those concepts (edges)
data ConceptGraph b a =
  ConceptGraph [Concept a] [ConceptDependency b a]
  deriving (Data,Typeable)

-- | Concept lattice is a special kind of graph
type ConceptLattice b a = ConceptGraph b a

instance (Show b, Show a) => Show (ConceptGraph b a) where
  show (ConceptGraph concepts deps) = "\n" ++ show concepts ++ "\n\n" ++ show deps

concepts :: ConceptGraph b a -> [a]
concepts (ConceptGraph c _) = map (\(Concept x) -> x) c

edges :: ConceptGraph b a -> [ConceptDependency b a]
edges (ConceptGraph _ e) = e
