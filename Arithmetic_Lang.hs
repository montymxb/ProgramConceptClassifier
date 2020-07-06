--
-- Arithmetic_Lang.hs
--
-- Representation of a simplistic arithmetic language's syntax
--

import Grammar
import Node
import Rule
import KnowledgeBase

--
-- symbols
--
e :: Node
e = sym (Term "E") [(TextAndCode "Expression")]

-- variable usage
varu :: Node
varu = sym (Use "u") [(TextAndCode "Usage of a bound variable")]

-- variable definition
vard :: Node
vard = sym (Def "v") [(TextAndCode "Binding a name to an expression")]

-- integer
i :: Node
i = sym (Term "I") [(TextAndCode "1,2,3,4,5")]

-- operator
o :: Node
o = sym (Term "O") [(TextAndCode "<,>,+,-,*,/,=")]

-- plus
p :: Node
p = sym (Term "+") [(TextAndCode "Adds two integers together")]

-- minus
m :: Node
m = sym (Term "-") [(TextAndCode "Subtracts one integer from another")]

-- multiplication
x :: Node
x = sym (Term "*") [(TextAndCode "Multiplies two integers together")]

-- division
d :: Node
d = sym (Term "/") [(TextAndCode "Divided one integer by another")]

-- or
orr :: Node
orr = sym (Term "|") [(TextAndCode "Either the first or the second is true?")]

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
  opRule]
