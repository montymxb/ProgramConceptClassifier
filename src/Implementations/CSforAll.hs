--
-- CSforAll Concept Relationships
--

module Implementations.CSforAll where

import Grammar.Symbol
import Grammar.Rule
import Grammar.Grammar
import Grammar.GrammarToGraph
import ConceptGraph.GraphToConceptGraph
import ConceptGraph.ConceptGraph
import Query.Query

-- Individual nodes
algorithm       = NonTerminal "Algorithm" []
output          = Terminal "Output" []
instructions    = NonTerminal "Instructions" []
conditions      = Terminal "Conditions" []
input           = Terminal "Input" []
representations = NonTerminal "Representations" []
names           = Terminal "Names" []
-- not really a Term, but putting it like that to avoid the loop
abstraction     = Terminal "Abstraction" []
resources       = Terminal "Resources" []
computer        = NonTerminal "Computer" []
computation     = NonTerminal "Computation" []

-- Relationships
algoRule = Rule algorithm [
  RHS "receives" [input],
  RHS "consists of" [instructions],
  RHS "produces" [output],
  RHS "uses" [representations]
  ]
instructionsRule = Rule instructions [
  RHS "use" [conditions]
  ]

representationsRule = Rule representations [
  RHS "creates" [abstraction],
  RHS "referred to by" [names]
  ]

computerRule = Rule computer [
  RHS "performs/produces" [computation],
  RHS "runs/exectues" [algorithm]
  ]

computationRule = Rule computation [
  RHS "changes/tranforms" [representations],
  RHS "requires/takes up" [resources]
  ]

-- | CSforAll word poster as a 'grammar'
concept_rep = Grammar "CSforAll Word Poster" [
  algoRule,
  instructionsRule,
  representationsRule,
  computerRule,
  computationRule
  ]

-- | Concept graph from this grammar representation
concept_graph = graph_to_concept_graph (grammar_to_graph concept_rep)

-- | Query from names -> computer
q1 = query names computer

-- | Performs a query on the concept graph of CSforAll items
csforall_query q = queryGraph concept_graph q
