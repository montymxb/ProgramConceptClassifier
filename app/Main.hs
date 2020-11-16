module Main where

main :: IO ()
main = return ()
--
-- Tests the query systems
-- > test (FindOneContaining ["Int"])
--
{-
test :: Query -> IO ()
test qq = do
  putStrLn "\n\n==Query Examples==\n\n"

  putStrLn $ show qq ++ " produces...\n"
  putStrLn $ present $ runProgQuery qq
  let c = (ihtml qq [] (present $ runProgQuery qq))

  writeFile "Suggestions.html" (html $ (lhtml concepts) ++ c)

  return ()
  -}
