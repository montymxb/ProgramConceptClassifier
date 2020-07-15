--
-- BOGL_Lang.hs
--
-- Simplified representation of BOGL's syntax
--

import Symbol
import Rule
import Grammar


bogl_grammar :: Grammar
bogl_grammar = Grammar "BOGL Lang" []


{--
Examples of this language
game TestGame

type Board = Array(1,1) of Int
type Input = Int

func : Int
func = 32


--}
