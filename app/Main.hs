module Main where

import R32
import Bogl_Specifics

main :: IO ()
main = r32 (FCA
  parseBOGLPrograms
  boglConceptMapping --asIsConceptMapping
  [("K1","game S\nv : Int\nv = 2")] -- ("K1","game S\nv : Int\nv = 3 + 3"),("K2","game S\nv : Int\nv = 3")
  [("GOAL","game E\nv : Int\nv = if True == False then 2 * 5 + 2 else let q = 2 in q")] -- ("GOAL","game E\nv : Int\nv = if True == False then 2 * 5 + 2 else let q = 2 in q")
  exConcretePrograms
  []
  [])
