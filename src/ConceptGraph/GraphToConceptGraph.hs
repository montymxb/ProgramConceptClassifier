--
-- Converts a graph to a concept graph
--

module ConceptGraph.GraphToConceptGraph (graph_to_concept_graph) where

import ConceptGraph.Concept
import ConceptGraph.ConceptDependency
import ConceptGraph.ConceptGraph

-- | Constructs a ConceptGraph from a provided list of vertex concepts and dependencies amongst them
graph_to_concept_graph :: ([a],[(b,a,a)]) -> ConceptGraph b a
graph_to_concept_graph (syms, deps) = let x = map concept syms in
                                      let y = map conceptdep deps in
                                      (ConceptGraph x y)
