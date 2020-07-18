--
-- Arithmetic_Lang.hs
--
-- Representation of a simplistic arithmetic language's syntax
--

import Symbol
import Rule
import Grammar
import GrammarToGraph
import GraphToConceptGraph
import ConceptGraph
import Query

--
-- symbols
--
f = NonTerminal "F" ["Fake symbol for testing"]

e = NonTerminal "E" ["Expression"]

-- variable usage
varu = Terminal "u" ["Usage of a bound variable"]

-- variable definition
vard = Terminal "v" ["Binding of a name to an expression"]

-- integer
i = Terminal "i" ["1,2,3,4,5","Integers"]

-- operator
o = NonTerminal "O" ["<,>,+,-,*,/,=","Operators"]

-- plus
p = Terminal "+" ["Adds 2 integers"]

-- minus
m = Terminal "-" ["Subtraces one integer from another"]

-- multiplication
x = Terminal "*" ["Multiplies 2 integers"]

-- division
d = Terminal "/" ["Divide one integer by another"]

-- or
orr = Terminal "|" ["Either the first of the second is true"]

--
-- rules
--

-- int represents an expr
exprRule :: Rule
exprRule = Rule e [
  RHS "Single Int" [i],                   -- ex: 42
  RHS "Variable" [varu],                  -- ex: var
  RHS "Two Exprs with Operator" [e,o,e],  -- ex: 1 + 2
  RHS "Let Expression" [vard,e,e]]        -- ex: let var := e1 in e2

--nameRule :: Rule
--nameRule = Rule "V" [
--  RHS "Variable can resolve to an expression" [e]]

-- operator can be one of many
opRule :: Rule
opRule = Rule o [
  RHS "Plus"  [p],
  RHS "Minus" [m],
  RHS "Mult"  [x],
  RHS "Div"   [d],
  RHS "Or"    [orr]
  ]

fakeRule :: Rule
fakeRule = Rule f [
  RHS "FakeRule" [e]
  ]

fakeRule2 :: Rule
fakeRule2 = Rule f [
  RHS "FakeRule2" [o]
  ]

{--
Examples of this language
5
1 + 2
3 * 4
12 / 4
x ::= 1
y ::= 2 + 3
z ::= x + y
a ::= x | z


--}

-- representation of the simple arithmetic grammar
arithmetic_grammar_rep :: Grammar
arithmetic_grammar_rep = Grammar "Arithmetic Toy Lang" [
  exprRule,
  opRule
  --fakeRule,
  --fakeRule2
  ]

-- gen a concept graph
arithmetic_concept_graph :: ConceptGraph GrammarDependency Symbol
arithmetic_concept_graph = graph_to_concept_graph (grammar_to_graph arithmetic_grammar_rep)

-- a test query
-- From 'o' to 'f'
q1 :: Query Symbol
q1 = query o e

-- example query on this representation of the arithmetic language
arith_query :: Query Symbol -> [Maybe (Path Symbol)]
arith_query q = queryGraph arithmetic_concept_graph q
