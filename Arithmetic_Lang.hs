--
-- Arithmetic_Lang.hs
--
-- Representation of a simplistic arithmetic language's syntax
--

import AL_CFG

--
-- symbols
--
e :: Symbol
e = NT "E"

i :: Symbol
i = T "i"

-- operator
o :: Symbol
o = NT "O"

-- plus
p :: Symbol
p = T "+"

-- minus
m :: Symbol
m = T "-"

-- multiplication
x :: Symbol
x = T "x"

-- division
d :: Symbol
d = T "/"

-- or
orr :: Symbol
orr = T "|"

--
-- rules
--

-- int represents an expr
exprRule :: Rule
exprRule = Rule "E" [
  RHS "Single Int" [i],
  RHS "Two Exprs with Operator" [e,o,e]]

-- operator can be one of many
opRule :: Rule
opRule = Rule "O" [
  RHS "Plus" [p],
  RHS "Minus" [m],
  RHS "Mult"[x],
  RHS "Div" [d],
  RHS "Or" [orr]
  ]

arithmetic_grammar :: Grammar
arithmetic_grammar = Grammar "Arithmetic Toy Lang" [
  exprRule,
  opRule]
