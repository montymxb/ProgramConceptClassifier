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
e = SE (Term "E") (Text "Expression")

-- variable usage
varu :: Symbol
varu = SE (Use "u") (Text "Usage of a bound variable")

-- variable definition
vard :: Symbol
vard = SE (Def "v") (Text "Binding a name to an expression")

i :: Symbol
i = SE (Term "I") (Code "1,2,3,4,5")

-- operator
o :: Symbol
o = SE (Term "O") (Code "<,>,+,-,*,/,=")

-- plus
p :: Symbol
p = SE (Term "+") (Text "Adds two integers together")

-- minus
m :: Symbol
m = SE (Term "-") (Text "Subtracts one integer from another")

-- multiplication
x :: Symbol
x = SE (Term "*") (Text "Multiplies two integers together")

-- division
d :: Symbol
d = SE (Term "/") (Text "Divided one integer by another")

-- or
orr :: Symbol
orr = SE (Term "|") (Text "Either the first or the second is true?")

--
-- rules
--

-- int represents an expr
exprRule :: Rule
exprRule = Rule e [
  RHS "Single Int" [i],                   -- ex: 42
  RHS "Variable" [varu],                  -- ex: var
  RHS "Two Exprs with Operator" [e,o,e],  -- ex: 1 + 2
  RHS "Let Expression" [vard,e,e]]        -- ex: let var = e1 in e2

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

arithmetic_grammar :: Grammar
arithmetic_grammar = Grammar "Arithmetic Toy Lang" [
  exprRule,
  opRule]
