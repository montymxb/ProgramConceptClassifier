module Main where

import Programs.BOGL_R1
import ConceptGraph.Conceptual
import ConceptGraph.ConceptGraph
import ConceptGraph.ManualConcepts as MC

import P2021.R32
import P2021.Bogl_Specifics

main :: IO ()
main = r32 (FCA
  OrderByExtents
  AllConcepts
  parseBOGLPrograms
  boglConceptMapping --asIsConceptMapping
  [] -- ("KNOWN","game S")
  [] -- ("GOAL","game T\nid : Int -> Int\nid(x) = x")
  exConcretePrograms)

  -- ("KNOWN","game K\nv : Bool\nv = True")
  -- ("GOAL","game G\nv : Bool\nv = if True == False then False else True")
