--
-- BoglPrograms.hs
-- Database of tagged bogl programs
--

module ProgramDatabase.BoglPrograms(queryProgs,allProgramParts,runProgQuery,Query(..),present,ihtml,lhtml,html,concepts) where

import Data.List
import ProgramDatabase.Database

--
-- Concepts
--
--c_game = "Game"

-- type categories
type TypeCategory = (String,[String])

c_val = "Value Equation"
c_fun = "Function Equation"

c_typ = "Type"
c_sym = "Symbol"
c_set = "Set"

c_int = "Int"
c_bool = "Bool"
c_tuple = "Tuple"

c_board = "Board"
c_array = "Array"

c_ext = "Extended Type"
c_extbase = "Ext. Base"
c_extset = "Ext. Set"

c_pos = "X and Y Position"

c_input = "Input"
c_boarddef = "Board Def"

c_get = "Get"
c_posvar = "Position Variable"
c_poslit = "Position Literal"
c_assign = "Assigning a value to a Position"
c_manyeqs = "Many Board Equations"
c_valtopos = "Assigning a value to a Position"

c_add = "Addition"
c_sub = "Subtraction"
c_mult = "Multiplication"
c_div = "Division"
c_mod = "Modulo"

c_eq = "Equality"
c_neq = "Non Equality"
c_ge = "Greater Than"
c_le = "Less Than"
c_geq = "Greater Than Equal To"
c_leq = "Less Than Equal To"

c_paren = "Parentheses (Order of Ops)"
c_if = "If Then Else"
c_let = "Let Expression"
c_ref = "Reference"
c_app = "Func Application"
c_while = "While"

concepts = [
  ("Type",  [
      c_typ,
      c_int,
      c_bool,
      c_sym,
      c_tuple,
      c_set]),

  ("Extended Types", [
      c_ext,
      c_extbase,
      c_extset]),

  ("Board", [
      c_board,
      c_array,
      c_pos]),

  ("Input", [
      c_input]),

  ("Board Def", [
      c_boarddef,
      c_get,
      c_posvar,
      c_poslit,
      c_valtopos,
      c_manyeqs]),

  ("Integer Expressions", [
      c_add,
      c_sub,
      c_mult,
      c_div,
      c_mod]),

  ("Boolean Expressions", [
      c_eq,
      c_neq,
      c_ge,
      c_le,
      c_geq,
      c_leq]),

  ("General Expressions", [
      c_paren,
      c_if,
      c_let,
      c_ref,
      c_while]),

  ("Value Defs",[
      c_val]),

  ("Func Defs",[
      c_fun,
      c_app])
  ]

--
-- Program Concept Pairings
--

-- game decl
a = [("game Example",[])]

-- type decls
b = [("type T1 = {X}",[c_typ,c_sym]),
      ("type T2 = {X,Y}", [c_typ,c_sym,c_set]),
      ("type T3 = Int", [c_typ,c_int]),
      ("type T4 = Int & {Empty}", [c_typ,c_int,c_sym,c_set,c_ext])]

-- board decls
c = [("type Board = Array(1,1) of Int", [c_typ,c_board,c_array,c_int]),
  ("type Board = Array(1,1) of Bool", [c_typ,c_board,c_array,c_bool]),
  ("type Board = Array(1,1) of T2", [c_typ,c_board,c_array])]

-- input decls
i = [("type Input = Int", [c_typ,c_input,c_int]),
  ("type Input = Bool", [c_typ,c_input,c_bool]),
  ("type Input = T2", [c_typ,c_input,c_sym])]

-- value defs
v = [
  -- int stuff
  ("v : Int\nv = 1", [c_val,c_int]),
  ("v : Int\nv = 1 + 1", [c_val,c_int,c_add]),
  ("v : Int\nv = 5 - 2", [c_val,c_int,c_sub]),
  ("v : Int\nv = 2 * 2", [c_val,c_int,c_mult]),
  ("v : Int\nv = 6 / 3", [c_val,c_int,c_div]),
  ("v : Int\nv = 10 % 3", [c_val,c_int,c_mod]),
  ("v : Int\nv = 5 + 2 * 3", [c_val,c_int,c_add,c_mult]),
  ("v : Int\nv = (5 + 2) * 3", [c_val,c_int,c_add,c_mult,c_paren]),
  ("v : Int\nv = let x = 32 in x", [c_val,c_int,c_let]),
  ("v : Int\nv = let x = 1 in let y = 4 in x + y", [c_val,c_int,c_let,c_add]),
  ("v : Int\nv = let x = 2 in let y = 4 in x * y", [c_val,c_int,c_let,c_mult]),

  -- bool stuff
  ("v : Bool\nv = True", [c_val,c_bool]),
  ("v : Bool\nv = False", [c_val,c_bool]),
  ("v : Bool\nv = if True then True else False", [c_val,c_bool,c_if]),
  ("v : Bool\nv = if False then True else False", [c_val,c_bool,c_if]),
  ("v : Int\nv = if True then 1 else 0", [c_val,c_bool,c_int,c_if]),
  ("v : Int\nv = if False then 1 else 0", [c_val,c_bool,c_int,c_if]),
  ("v : Bool\nv = 1 == 1", [c_val,c_bool,c_int,c_int,c_eq]),
  ("v : Bool\nv = 1 /= 1", [c_val,c_bool,c_int,c_int,c_neq]),
  ("v : Bool\nv = 5 > 4", [c_val,c_bool,c_int,c_int,c_ge]),
  ("v : Bool\nv = 5 < 4", [c_val,c_bool,c_int,c_int,c_le]),
  ("v : Bool\nv = 5 >= 4", [c_val,c_bool,c_int,c_int,c_geq]),
  ("v : Bool\nv = 5 <= 4", [c_val,c_bool,c_int,c_int,c_leq]),
  ("v : Bool\nv = 5-4 < 4+2", [c_val,c_bool,c_int,c_int,c_ge,c_sub,c_add]),
  ("v : Bool\nv = 5-4 > 4+2", [c_val,c_bool,c_int,c_int,c_le,c_sub,c_add]),
  ("v : Bool\nv = if 1==1 then True else False", [c_val,c_bool,c_int,c_if,c_eq]),
  ("v : Bool\nv = if 1==0 then True else False", [c_val,c_bool,c_int,c_if,c_eq]),
  ("v : Bool\nv = if 1/=1 then True else False", [c_val,c_bool,c_int,c_if,c_neq]),
  ("v : Bool\nv = if 1/=0 then True else False", [c_val,c_bool,c_int,c_if,c_neq]),

  ("v : Bool\nv = let x = 5 in if x+1 > 5 then True else False", [c_val,c_bool,c_int,c_if,c_let,c_ge]),

  -- symbol stuff
  ("v : T2\nv = X", [c_val,c_typ,c_sym]),
  ("v : T2\nv = let r = X in r", [c_val,c_typ,c_sym,c_let]),
  ("v : T2\nv = if True then X else Y", [c_val,c_typ,c_sym,c_if]),

  -- referencing other equations
  ("v : Int\nv = 5\nv2 : Int\nv2 = v", [c_val,c_int,c_ref])]

-- board equations
be = [("b : Board\nb!(1,1) = 1", [c_val,c_board,c_get,c_pos,c_int,c_assign,c_poslit]),
  ("b : Board\nb!(x,1) = 1", [c_val,c_board,c_get,c_pos,c_int,c_assign,c_posvar,c_poslit]),
  ("b : Board\nb!(1,y) = 1", [c_val,c_board,c_get,c_pos,c_int,c_assign,c_posvar,c_poslit]),
  ("b : Board\nb!(x,y) = 1", [c_val,c_board,c_get,c_pos,c_int,c_assign,c_posvar]),
  ("b : Board\nb!(x,y) = 1\nb!(1,1) = 0", [c_val,c_board,c_get,c_pos,c_int,c_assign,c_posvar,c_poslit,c_manyeqs])]

-- function defs
f = [("f : Int -> Int\nf(x) = x", [c_fun,c_int,c_ref]),
  ("f : Int -> Int\nf(x) = x+1", [c_fun,c_int,c_ref,c_add]),
  ("f : Int -> Bool\nf(x) = x == 0", [c_fun,c_int,c_bool,c_ref,c_eq]),
  ("f : Int -> Bool\nf(x) = if x /= 0 then True else False", [c_fun,c_int,c_bool,c_ref,c_neq,c_if]),
  ("f : (Int,Int) -> Int\nf(x,y) = x+y", [c_fun,c_tuple,c_int,c_ref,c_add]),
  ("f : (Bool,Int) -> Int\nf(x,y) = if x then y else 0", [c_fun,c_tuple,c_int,c_ref,c_bool,c_if]),
  ("f : (Int,Int,Board) -> Int\nf(x,y,b) = b!(x,y)", [c_fun,c_tuple,c_int,c_board,c_ref,c_get])]

-- list of all program pieces we can build with
allProgramParts :: [(Statement,[Concept])]
allProgramParts = concat [a,b,c,i,v,be,f]

-- takes a list of concepts, produces a list of tuples of ([Statement],[Concepts])
-- produce all permutations of items, using at 1st just the one, then the 2nd, and so on

-- names of the queries to use
queryNames = ["FindOneExact","FindAllExact","FindOneContaining","FindAllContaining","FindOneAny","FindAllAny","FindInOrder","FindAllOrders"]

-- basic query type
type Concept = String
data Query = FindOneExact [Concept] -- find one exact match for this set of concepts
  | FindAllExact [Concept]          -- find all exact matches for this set of concepts
  | FindOneContaining [Concept]     -- find one match containing this set of concepts
  | FindAllContaining [Concept]     -- find all matches containing this set of concepts
  | FindOneAny [Concept]            -- find one match that has any concept from this set
  | FindAllAny [Concept]            -- find all matches that have any concept from this set
  | FindInOrder [Concept]           -- find a set of programs using the first any match for each concept in the set (NOT w/ respect to inter-dependencies)
  | FindAllOrders [Concept]         -- find all a sets of programs that would work for order (WIP)
  deriving Show

-- TODO !!!!!
-- Change 'Queries' to only run on lists of concepts, don't care about the rest...
-- also, add some quick logic to handle singular/plural searches...otherwise I'm just writing crap all the time
-- Add to the printout a thing that says satisfies 'Concept', and uses: ...
--  this way we can tell what matched, and what did not...for our query
-- htmlify, and then sit my butt down for the night...

first :: [(Statement,[Concept])] -> [(Statement,[Concept])]
first []     = []
first (f:ls) = [f]

-- run a program query to get results from the 'DB'
runProgQuery :: Query -> [(Statement,[Concept])]
runProgQuery (FindOneExact s)       = first $ filter (\(_,cls) -> s == cls) allProgramParts
runProgQuery (FindAllExact s)       = filter (\(_,cls) -> s == cls) allProgramParts
runProgQuery (FindOneContaining s)  = first $ filter (\(_,cls) -> all (\x -> elem x cls) s) allProgramParts
runProgQuery (FindAllContaining s)  = filter (\(_,cls) -> all (\x -> elem x cls) s) allProgramParts
runProgQuery (FindOneAny s)         = first $ filter (\(_,cls) -> any ((flip elem) cls) s) allProgramParts
runProgQuery (FindAllAny s)         = filter (\(_,cls) -> any ((flip elem) cls) s) allProgramParts
runProgQuery (FindInOrder s)        = queryProgs s allProgramParts
runProgQuery (FindAllOrders s)      = undefined



type Statement = String
queryProgs :: [Concept] -> [(Statement,[Concept])] -> [(Statement,[Concept])]
queryProgs [] _         = []
queryProgs (c:ls) progs = let found = find (\(_,cls) -> elem c cls) progs in
                          case found of
                            (Just f) -> let progs2 = filter (\x -> x /= f) progs in
                                        f : queryProgs ls progs2
                            Nothing  -> queryProgs ls progs -- skip to the next one error $ "Could not find concept " ++ (show c) ++ "!"

-- 1.) Filter out concepts that have no significance to this topic?
--generateAll concepts progs =

topFrame = "<!DOCTYPE html><html><head><meta charset='utf-8'><title>Program Decomposition Tool</title><link href=\"css/site.css\" type=\"text/css\" rel=\"stylesheet\"/></head><body>"
bottomFrame = "</body><script src='js/site.js' async></script></html>"

-- comma separate values
commasep :: [String] -> String
commasep []      = ""
commasep (c:ls)  = c ++ ", " ++ commasep ls

-- construct a program representation, w/ concepts inlined
construct :: [String] -> [[String]] -> String
construct [] _    = ""
construct (s:ls) (c:ls2)= "--" ++ (commasep c) ++ "\n" ++ s ++ "\n\n" ++ construct ls ls2

-- present this information
present :: [(Statement,[Concept])] -> String
present parts = let p1s = map fst parts in
           let c1s = map snd parts in
           let content = construct p1s c1s in
           content

buttons :: [String] -> String
buttons []      = ""
buttons (c:ls)  = "<div class='button'>" ++ c ++ "</div>" ++ buttons ls

html :: String -> String
html s = topFrame ++ s ++ bottomFrame

htmlButtonGroups :: [(String,[String])] -> String
htmlButtonGroups ls = foldl (\ss (groupName,ls) -> ss ++ "<div class='grouping'><span class='groupName'>" ++ groupName ++ "</span>" ++ (foldl (\s x -> s ++ "<div class='button'>"++ x ++"</div>") "" ls) ++ "</div>") "" ls

-- construct HTML sections for the program decomposition to be displayed within
lhtml :: [(String,[String])] -> String
lhtml ls = "<textarea id='program-decomp' placeholder='Enter a program here.'></textarea><div id='decomposition'></div><div class='concept-list'>" ++ (htmlButtonGroups ls) ++ "<br/><br/>" ++ (foldl (\s x -> s ++ "<div class='qbutton'>"++ x ++"</div>") "" queryNames) ++ "<textarea id='query-field' placeholder=''></textarea></div>"

ihtml :: Query -> [String] -> String -> String
ihtml q ls s = "<div class='query'><div class='query-name'>" ++ show q ++ "</div><div class='tray'>" ++ buttons ls ++ "</div><div class='bogl-code'>" ++ s ++ "</div></div>"