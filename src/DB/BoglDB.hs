module DB.BoglDB where

import System.Directory
import Parser.Parser
import Data.Either
import ConceptGraph.Conceptual
import ConceptGraph.ManualConcepts as MC
import Language.Syntax (Game)
import Text.Parsec.Pos
import Debug.Trace

{-
parseFile :: String -> IO (Maybe (Game SourcePos))
parseFile fn = do
  rez <- parseGameFile fn
  case rez of
    Right g -> return (Just g)
    Left _  -> return (Nothing)
-}

{-
--getResults :: [IO (Maybe (Game SourcePos))] -> [IO (Game SourcePos)]
getResults :: Monad m => [IO (Maybe a)] -> [m a]
getResults [] = []
getResults (x:ls) = do
  let (Just g) = x
  (return g) : (getResults ls)
  --rez <- x
  --case x of
  --  Just g  -> [] --(return g) : (getResults ls)
  --  Nothing -> [] --getResults ls
-}

{-
db_programs :: IO ()
db_programs = do
  files <- getDirectoryContents "db_programs"
  let maybeProgs = map parseFile files
  --let progs = getResults eitherProgs
  putStrLn $ show files
-}


-- write up a pair of programs for Bogl
p1 :: String
p1 = "game EX1\nfive : Int\nfive = 5"

p2 :: String
p2 = "game EX2\ninc : Int -> Int\ninc(x) = x + 1"

db_programs :: [(Game SourcePos,String)]
db_programs = let progs = [p1,p2] in
              let games = rights $ map (\x -> parsePreludeAndGameText "" x "test") [p1,p2] in
              zip games progs

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
