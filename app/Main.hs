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

test :: Query -> IO ()
test qq = do
  putStrLn "\n\n==Query Examples==\n\n"

  {-
  let q = (FindOneExact ["Game"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c1 = (ihtml q ["Game"] (present $ runProgQuery q))
  putStrLn "========================="

  let q = (FindAllExact ["Game"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c2 = (ihtml q ["Game"] (present $ runProgQuery q))
  putStrLn "========================="

  let q = (FindOneExact ["Game","Board","Int","Addition"])
  putStrLn $ show q ++ " produces...(nothing)\n"
  putStrLn $ present $ runProgQuery q
  let c3 = (ihtml q ["Game","Board","Int","Addition"] (present $ runProgQuery q))
  putStrLn "========================="

  let q = (FindOneExact ["Type","Input","Int"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c4 = (ihtml q ["Type","Input","Int"] (present $ runProgQuery q))
  putStrLn "========================="

  let q = (FindAllExact ["Type","Input","Int"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c5 = (ihtml q ["Type","Input","Int"] (present $ runProgQuery q))
  putStrLn "========================="

  let q = (FindOneContaining ["Addition","Int"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c6 = (ihtml q ["Addition","Int"] (present $ runProgQuery q))
  putStrLn "========================="

  let q = (FindAllContaining ["Addition","Int"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c7 = (ihtml q ["Addition","Int"] (present $ runProgQuery q))
  putStrLn "========================="

  let q = (FindOneAny ["Addition","Multiplication"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c8 = (ihtml q ["Addition","Multiplication"] (present $ runProgQuery q))
  putStrLn "========================="

  let q = (FindAllAny ["Addition","Multiplication"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c9 = (ihtml q ["Addition","Multiplication"] (present $ runProgQuery q))
  putStrLn "========================="
  -}

  {-
  let q = (FindOneExact ["Game"])
  putStrLn $ show q ++ " produces...\n"
  putStrLn $ present $ runProgQuery q
  let c10 = (ihtml q ["Game","Addition","Value","If Then Else","Let Expression","Function"] (present $ runProgQuery q))
  putStrLn "========================="
  -}

  putStrLn $ show qq ++ " produces...\n"
  putStrLn $ present $ runProgQuery qq
  let c11 = (ihtml qq [] (present $ runProgQuery qq))

  writeFile "Suggestions.html" (html $ (lhtml concepts) ++ c11)

  --writeFile "Suggestions.html" (html $ (lhtml concepts) ++ c1 ++ c2 ++ c3 ++ c4 ++ c5 ++ c6 ++ c7 ++ c8 ++ c9 ++ c10)

  return ()
