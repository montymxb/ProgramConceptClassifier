module Main where

import ProgramConceptClassifier
import Bogl_Specifics
import System.Directory
import System.Process
import Toy

--getBGLFileFromDir :: String -> String -> (String,IO String)
getFileFromDir :: String -> String -> String -> IO (String, String)
getFileFromDir ext dir s = do
  content <- readFile $ dir ++ s ++ ext
  return (s,content)

tail4 :: [a] -> [a]
tail4 = tail . tail . tail . tail

getAllBGLFilesFromDir :: String -> IO [(String, String)]
getAllBGLFilesFromDir dir = do
  files <- listDirectory dir
  let f2 = map (\x -> reverse $ tail4 $ reverse x) (filter (filterFile . reverse) files)
  bgls <- mapM (\x -> getFileFromDir ".bgl" dir x) f2
  return bgls
  where
    filterFile :: String -> Bool
    filterFile ('l':'g':'b':'.':_) = True
    filterFile _ = False

main :: IO ()
main = do

  let dir = "db_programs/"

  let getSimpleBGLFile = getFileFromDir ".bgl" (dir ++ "simple/")
  let getGameBGLFile = getFileFromDir ".bgl" (dir ++ "games/")

  bglFiles1 <- getAllBGLFilesFromDir (dir ++ "simple2/")
  bglFiles2 <- getAllBGLFilesFromDir (dir ++ "games/")
  let bglFiles = rightProgs $ parseBOGLPrograms $ bglFiles1 ++ bglFiles2

  -- example programs to use for known/goal
  k1 <- getSimpleBGLFile "Simplest"
  g1 <- getSimpleBGLFile "Factorial"

  let known = rightProgs $ parseBOGLPrograms [k1]
  putStrLn $ show known

  -- g2 <- getGameBGLFile "tictactoe"
  let goal = rightProgs $ parseBOGLPrograms [g1] -- another option, ("G1","game S\nv : Int\nv = let x = 24 in 24 * 2 + 5 - x")

  -- fringe unused in this analysis result
  let (dotContent,_) = analyze (MappablePrograms
        boglConceptMapping
        known
        goal
        bglFiles)

  -- write GV spec
  writeFile "AnalysisDOT.gv" dotContent
  _ <- system ("dot -Tpng -oAnalysisLattice.png AnalysisDOT.gv")
  return ()
