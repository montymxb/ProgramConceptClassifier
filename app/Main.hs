module Main where

import FormalConceptAnalysis
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

  bglFiles1 <- getAllBGLFilesFromDir (dir ++ "simple2/")
  bglFiles2 <- getAllBGLFilesFromDir (dir ++ "games/")
  let bglFiles = rightProgs $ parseBOGLPrograms $ bglFiles1 ++ bglFiles2

  --k1 <- getSimpleBGLFile "Simplest" -- Notakto
  --k2 <- getSimpleBGLFile "V_Ref"
  --k3 <- getSimpleBGLFile "V_Sub"
  --k4 <- getSimpleBGLFile "V_Add"
  --k5 <- getSimpleBGLFile "Input1"
  --k6 <- getSimpleBGLFile "V_AddSub"
  k7 <- getSimpleBGLFile "Factorial"

  ct <- getGameBGLFile "CoinToss"

  let known = rightProgs $ parseBOGLPrograms [ct]
  putStrLn $ show known
  -- k1,k2,k3,k4,k5,k6,k7 has empty classification!

  -- <- getSimpleBGLFile "tictactoe" -- tictactoe
  g2 <- getGameBGLFile "tictactoe"
  let ex = ("G1","game S\nf : Int -> Int\nf(x) = if x > 1 then x * f(x-1) else 1\nt : Bool\nt = True\nv : Int\nv = let x = 24 in 24 * 2 + 5 - x")
  let goal = rightProgs $ parseBOGLPrograms [g2] -- ("G1","game S\nv : Int\nv = let x = 24 in 24 * 2 + 5 - x")

  let extraProgs    = []
  let extraAttribs  = []

  (dotContent,fringe) <- fca (FCA
        boglConceptMapping
        known
        goal
        bglFiles
        extraProgs
        extraAttribs)

  {-
  putStrLn $ "Program Count: " ++ show (length bglFiles)
  putStrLn $ "Fringe: " ++ show fringe
  putStrLn dotContent
  -}

  -- write GV spec
  writeFile "R32_Test_1.gv" dotContent
  _ <- system ("dot -Tpng -oR32_Test_1.png R32_Test_1.gv")
  return ()
