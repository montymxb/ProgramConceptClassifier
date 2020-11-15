module Main where

import Lib
--import Implementations.BOGL_Lang as BOGL

--import Implementations.Arithmetic_Lang as AL
--import Implementations.CSforAll as CSA
--import Implementations.BOGL_AST_TC as BOGL_2
--import Implementations.T2 as T2

import AbstractSyntax.BOGL
import AbstractSyntax.Simple as Simple
import AST.BOGL_AST as BOGL
import ProgramDatabase.BoglPrograms

main :: IO ()
main = someFunc

--
-- Tests the query systems
-- > test (FindOneContaining ["Int"])
--
test :: Query -> IO ()
test qq = do
  putStrLn "\n\n==Query Examples==\n\n"

  putStrLn $ show qq ++ " produces...\n"
  putStrLn $ present $ runProgQuery qq
  let c = (ihtml qq [] (present $ runProgQuery qq))

  writeFile "Suggestions.html" (html $ (lhtml concepts) ++ c)

  return ()
