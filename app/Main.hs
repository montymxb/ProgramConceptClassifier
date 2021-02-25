module Main where

import R32
import Bogl_Specifics

main :: IO ()
main = r32 (FCA
  AllConcepts
  parseBOGLPrograms
  boglConceptMapping --asIsConceptMapping
  [("K1","game S\nv : Int\nv = 3 + 3"),("K2","game S\nv : Int\nv = 3")] -- ("K1","game S\nv : Int\nv = input"),("K2","game S")
  [("GOAL","game E\nv : Int\nv = if True == False then 2 * 5 + 2 else let q = 2 in q")]
  -- ("GOAL","game E\nv : Int\nv = if True == False then 2 * 5 + 2 else let q = 2 in q")
  exConcretePrograms
  [] -- extra progs by name
  -- "V_Int","V_Ref","V_Let1","V_Add","V_Mul"
  -- "V_ITE4","V_ITE1","V_AddMul","Input1","V_Let2"
  []) -- extra attributes by name
  -- IntExpr,ValueEquation,Type,PlainType,Value,BinOp,Ref,Let,Mult,Add
  -- BoolExpr,IfThenElse,Fls,Tru,Equiv

-- ("KNOWN","game S"),("B","game B\nv : Int\nv = 32"),("C","game E\nv : Int\nv = 2 + 1")
-- ("GOAL","game E\nv : Int\nv = 2 + 5 * 1")

-- ("KNOWN","game K\nv : Bool\nv = True")
-- ("GOAL","game G\nv : Bool\nv = if True == False then False else True")
