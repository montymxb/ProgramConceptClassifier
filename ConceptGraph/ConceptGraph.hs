--
-- Concept Graph describing the relationships amongst
-- concepts in a given context, with relationships established via
-- a grammar's rule set
--

module ConceptGraph (ConceptGraph(ConceptGraph),ConceptLattice) where

import Concept
import ConceptDependency

-- | ConceptGraph type, represents a graph with a list of concepts (vertices) and depedencies amongst those concepts (edges)
data ConceptGraph b a =
  ConceptGraph [Concept a] [ConceptDependency b a]

-- | Concept lattice is a special kind of graph
type ConceptLattice b a = ConceptGraph b a

instance (Show b, Show a) => Show (ConceptGraph b a) where
  show (ConceptGraph concepts deps) = "\n" ++ show concepts ++ "\n\n" ++ show deps
