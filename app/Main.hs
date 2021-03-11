module Main where

import R32
import Bogl_Specifics
import System.Directory
import System.Process


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
  --let dir = "/Users/Bfriedman/OSU/Research/ConceptGraph/db_programs/"
  --let dir160 = "/Users/Bfriedman/Downloads/CS160-BoGL-section/assignment_9_tictactoe/"
  --let dir160 = "/Users/Bfriedman/Downloads/CS160-BoGL-section/assignment_8_nim_board/"
  --let dir160 = "/Users/Bfriedman/Downloads/CS160-BoGL-section/sub_8/"

  let getSimpleBGLFile = getFileFromDir ".bgl" (dir ++ "simple/")
  let getGameBGLFile = getFileFromDir ".bgl" (dir ++ "games/")

  bglFiles1 <- getAllBGLFilesFromDir (dir ++ "simple/")
  bglFiles2 <- getAllBGLFilesFromDir (dir ++ "games/")
  let bglFiles = rightProgs $ parseBOGLPrograms $ bglFiles1 ++ bglFiles2

  k1 <- getSimpleBGLFile "Simplest" -- Notakto
  k2 <- getSimpleBGLFile "V_Ref"
  k3 <- getSimpleBGLFile "V_Sub"
  k4 <- getSimpleBGLFile "V_Add"
  k5 <- getSimpleBGLFile "Input1"
  k6 <- getSimpleBGLFile "V_AddSub"
  k7 <- getSimpleBGLFile "V_Let1"
  let known = rightProgs $ parseBOGLPrograms [k1]
  -- k1,k2,k3,k4,k5,k6,k7 has empty classification!

  g1 <- getSimpleBGLFile "V_LetAddSub" -- tictactoe
  g2 <- getGameBGLFile "tictactoe"
  let goal = rightProgs $ parseBOGLPrograms [] -- ("G1","game S\nv : Int\nv = let x = 24 in 24 * 2 + 5 - x")

  let extraProgs    = []
  let extraAttribs  = []



  dotContent <- r32 (FCA
        boglConceptMapping
        known
        goal
        bglFiles
        extraProgs
        extraAttribs)

  -- write GV spec
  writeFile "R32_Test_1.gv" dotContent
  _ <- system ("dot -Tsvg -oR32_Test_1.svg R32_Test_1.gv")
  return ()
