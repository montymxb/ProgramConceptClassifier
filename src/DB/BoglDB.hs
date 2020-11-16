module DB.BoglDB where

import System.Directory
import Parser.Parser
import Data.Either


{-
getResults [] = []
getResults (x:ls) = case x of
                      Right r -> r : (getResults ls)
                      Left l -> getResults ls

-- load up ALL examples
getDB dir = do
  files <- getDirectoryContents dir
  let bglFiles = map ((++) dir) files
  let parsedGames = map parseGameFile bglFiles
  -- get all the results for these
  let results = getResults parsedGames
  return (results)
-}
