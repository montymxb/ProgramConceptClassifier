module Main where

import Programs.BOGL_R1
import ConceptGraph.Conceptual
import ConceptGraph.ConceptGraph
import ConceptGraph.ManualConcepts as MC

import P2021.R32
import P2021.Bogl_Specifics

main :: IO ()
main = r32 (FCA
  OrderByIntents
  AllConcepts
  parseBOGLPrograms
  [] -- ("KNOWN","game S")
  [] -- ("GOAL","game T\nid : Int -> Int\nid(x) = x")
  exConcretePrograms)
