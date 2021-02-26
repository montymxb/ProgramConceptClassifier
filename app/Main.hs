module Main where

import R32
import Bogl_Specifics
import System.Directory


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

  let getBGLFile = getFileFromDir ".bgl" dir

  bglFiles <- getAllBGLFilesFromDir dir

  -- try and read the known from a file instead
  k1 <- getBGLFile "spaceinvaders_v2"
  let known = []

  -- try and do this with a goal file too
  g1 <- getBGLFile "tictactoe"
  let goal = [] -- ("G1","game S\nv : Int\nv = let x = 24 in 24 * 2 + 5 - x")

  let extraProgs    = []
  let extraAttribs  = []

  r32 (FCA
        parseBOGLPrograms
        boglConceptMapping
        known
        goal
        bglFiles
        extraProgs
        extraAttribs)
