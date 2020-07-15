--
-- Concept Graph describing the relationships amongst
-- concepts in a given context, with relationships established via
-- a grammar's rule set
--

module ConceptGraph (ConceptGraph(ConceptGraph)) where

import Concept
import ConceptDependency

data ConceptGraph b a =
  ConceptGraph [Concept a] [ConceptDependency b a]

instance (Show b, Show a) => Show (ConceptGraph b a) where
  show (ConceptGraph concepts deps) = "\n" ++ show concepts ++ "\n\n" ++ show deps
