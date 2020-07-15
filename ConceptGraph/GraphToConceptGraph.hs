--
-- Converts a graph to a concept graph
--

module GraphToConceptGraph (graph_to_concept_graph) where

import Concept
import ConceptDependency
import ConceptGraph

graph_to_concept_graph :: ([a],[(b,a,a)]) -> ConceptGraph b a
graph_to_concept_graph (syms, deps) = let x = map concept syms in
                                      let y = map conceptdep deps in
                                      (ConceptGraph x y)
