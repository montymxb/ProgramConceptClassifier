module Main where

import R32
import Bogl_Specifics

main :: IO ()
main = r32 (FCA
  OrderByExtents
  AllConcepts
  parseBOGLPrograms
  boglConceptMapping --asIsConceptMapping
  [("KNOWN","game S")] -- ("KNOWN","game S") -- ("KNOWN","game S")
  [("GOAL","game E\nv : Int\nv = if True == False then 2 * 5 + 2 else let q = 2 in q")] -- ("GOAL","game E\nv : Int\nv = 2 + 5") -- ("GOAL","game T\nid : Int -> Int\nid(x) = x")
  exConcretePrograms
  ["V_Int","V_Ref","V_Let1","V_Add","V_Mul"] -- extra progs by name
  -- "V_Int","V_Ref","V_Let1","V_Add","V_Mul"
  -- "V_ITE4","V_ITE1","V_AddMul","Input1","V_Let2"
  [IntExpr,ValueEquation,Type,PlainType,Value,BinOp,Ref,Let,Mult,Add]) -- extra attributes by name
  -- IntExpr,ValueEquation,Type,PlainType,Value,BinOp,Ref,Let,Mult,Add
  -- BoolExpr,IfThenElse,Fls,Tru,Equiv

-- ("KNOWN","game S"),("B","game B\nv : Int\nv = 32"),("C","game E\nv : Int\nv = 2 + 1")
-- ("GOAL","game E\nv : Int\nv = 2 + 5 * 1")

-- ("KNOWN","game K\nv : Bool\nv = True")
-- ("GOAL","game G\nv : Bool\nv = if True == False then False else True")
