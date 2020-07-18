--
-- CSforAll Concept Relationships
--

import Symbol
import Rule
import Grammar
import GrammarToGraph
import GraphToConceptGraph
import ConceptGraph
import Query

algorithm = NonTerminal "Algorithm" []
output = Terminal "Output" []
instructions = NonTerminal "Instructions" []
conditions = Terminal "Conditions" []
input = Terminal "Input" []
representations = NonTerminal "Representations" []
names = Terminal "Names" []
-- not really a Term, but putting it like that to avoid the loop
abstraction = Terminal "Abstraction" []

resources = Terminal "Resources" []
computer = NonTerminal "Computer" []
computation = NonTerminal "Computation" []

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

concept_rep = Grammar "CSforAll Word Poster" [
  algoRule,
  instructionsRule,
  representationsRule,
  computerRule,
  computationRule
  ]

concept_graph = graph_to_concept_graph (grammar_to_graph concept_rep)

q1 = query names computer

csforall_query q = queryGraph concept_graph q
