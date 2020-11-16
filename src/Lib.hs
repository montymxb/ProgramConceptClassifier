module Lib where

import Programs.BOGL_S1 as BoglProgsS1
import Programs.BOGL_R1 as BoglProgsR1
import ConceptGraph.Conceptual
import Data.Data
import DB.BoglDB
import Parser.Parser

adjustments :: [GraphAdjustment]
adjustments = [Rename "Btype" "Base Type",
              Rename "Parlist" "Parameter List",
              Pluck "Sig",
              Pluck "(Int,Int)"]

graphConcepts :: Data a => a -> String -> IO ()
graphConcepts val = let cg = conceptGraph val in
                    let adjustedCG = manuallyAdjustGraph adjustments cg in
                    graphConceptGraph adjustedCG

-- Simplified bogl program graphs
simp1 :: IO ()
simp1 = graphConcepts BoglProgsS1.p1 "simp1"

simp2 :: IO ()
simp2 = graphConcepts BoglProgsS1.p2 "simp2"

simp3 :: IO ()
simp3 = graphConcepts BoglProgsS1.p3 "simp3"

simp4 :: IO ()
simp4 = graphConcepts BoglProgsS1.p4 "simp4"

simp5 :: IO ()
simp5 = graphConcepts BoglProgsS1.p5 "simp5"

real1 :: IO ()
real1 = graphConcepts BoglProgsR1.boglProg "real1"

all1 :: IO ()
all1 = do
  simp1
  simp2
  simp3
  simp4
  simp5
  real1
  return ()

-- reads, parses and analyzes a bogl program to produce a concept lattice
analyzeBoglProgram :: String -> IO ()
analyzeBoglProgram fn = do
  rez <- parseGameFile fn
  case rez of
    (Right game) -> graphConcepts game fn
    (Left err)   -> error $ "Some parse error occurred. " ++ (show err)


-- get all files to read
--dbs = getDB "db_programs/"
