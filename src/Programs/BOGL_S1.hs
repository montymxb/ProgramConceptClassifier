{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ConstrainedClassMethods #-}
--
-- BOGL_P1
--

module Programs.BOGL_S1 where
  
import Data.Data
import Data.List
import ConceptGraph.Conceptual
import ConceptGraph.Concept
import AbstractSyntax.BOGL_S1

-- Don't Care Value
-- This is used to fill in a dummy value when BTypes are used to describe what type synonyms
-- equate to, and in the types for value and function equations. In these contexts, the existing AS for
-- a BType is the same as that used for typed values, and so a dummy describes this value has no meaning in this context
x_x :: Int
x_x = 000

-- p1, simplest game
{-
game P1

type Board = Array(1,1) of Int
type Input = Int
-}
p1 :: Game
p1 = (Game
  (UName "P1")
  []
  (Board 1 1 (BInt 1))
  (Input (BInt x_x))
  [])


-- p2, game with type and function that returns an Int of value 123
{-
game P2

type T1 = {E1}

type Board = Array(1,1) of Int
type Input = Int

f1 : Int
f1 = 123
-}
p2 :: Game
p2 = (Game
  (UName "P2")
  [
    (TOV_TypeAssign (TypeAssignXType (UName "T1") (X_EType (EType [(UName "E1")])))),
    (TOV_ValDef (ValDef (BSig (LName "f1") (BInt x_x)) (ValEquation (LName "f1") (IVal 123))))
  ]
  (Board 1 1 (BInt 1))
  (Input (BInt x_x))
  [])



-- practical examples


-- p3, factorial
{-
game Factorial_Example

type T1 = {E1}

type Board = Array(1,1) of Int
type Input = Int

type T2 = Board & {E2,E3}

fact : Int -> Int
fact(x) = if x > 1 then x * fact(x-1) else 1
-}
p3 :: Game
p3 =
  (Game
  (UName "Factorial_Example")
  [
    (TOV_TypeAssign (TypeAssignXType (UName "T1") (X_EType (EType [(UName "E1")])))),
    (TOV_TypeAssign (TypeAssignXType (UName "T2") (X_ExtEType (BBoard (Board 1 1 (BInt 1))) (EType [(UName "E2"),(UName "E3")]))))
  ]
  (Board 1 1 (BInt x_x))
  (Input (BInt x_x))
  [
    (ValDef
      (FSig (LName "fact") (FType (BInt x_x) (BInt x_x)))
      (FuncEquation (LName "fact") [(LName "x")] (Cond
        (BinOp (Ref (LName "x")) Greater (IVal 1))
        (BinOp (Ref (LName "x")) (Times)
          (App (LName "fact") [(BinOp (Ref (LName "x")) (Minus) (IVal 1))]))
        (IVal 1))))
  ])


-- TicTacToe
{-
game TicTacToe

type Player = {X, O}
type Space = Player & {Empty}
type Result = Player & {Tie}
type Position = (Int,Int)
type Bool = {True,False}
type E1 = {E1}
type T2 = Board & {E2,E3}

type Board = Array (3,3) of Space
type Input = Position

initialBoard : Board
initialBoard!(x,y) = Empty

next : Player -> Player
next(p) = if p == X then O else X

goFirst : Player
goFirst = X

outcome : (Player, Board) -> Result
outcome(p, b) = if inARow(3,X,b) then X else
                if inARow(3,O,b) then O else Tie

threeInARow : Board -> Bool
threeInARow(b) = or(inARow(3,X,b), inARow(3,O,b))

gameOver : Board -> Bool
gameOver(b) = or(threeInARow(b), isFull(b))

isValid : (Board,Position) -> Bool
isValid(b,p) = if b ! p == Empty then True else False

tryMove : (Player,Board) -> (Player, Board)
tryMove(p,b) = let pos = input in
                   if isValid(b,pos) then (next(p), place(p,b,pos))
                                     else (p, b)

loop : (Player,Board) -> (Player ,Board)
loop(p,b) = while not(gameOver(b)) do tryMove(p,b)

play : (Player,Board) -> Result
play(a,b) = outcome(loop(a,b))

result : Result
result = play(goFirst, initialBoard)

-}
p4 :: Game
p4 =
  (Game
  (UName "TicTacToe")
  [
    (TOV_TypeAssign (TypeAssignXType (UName "Player") (X_EType (EType [(UName "X"),(UName "O")])))),
    (TOV_TypeAssign (TypeAssignXType (UName "Space") (X_ExtEType (BUName (UName "Player")) (EType [(UName "Empty")])))),
    (TOV_TypeAssign (TypeAssignXType (UName "Result") (X_ExtEType (BUName (UName "Player")) (EType [(UName "Tie")])))),
    (TOV_TypeAssign (TypeAssignBType (UName "Position") (BTuple [(BInt x_x),(BInt x_x)]))),
    (TOV_TypeAssign (TypeAssignXType (UName "Bool") (X_EType (EType [(UName "True"),(UName "False")])))),
    (TOV_TypeAssign (TypeAssignXType (UName "T1") (X_EType (EType [(UName "E1")])))),
    (TOV_TypeAssign (TypeAssignXType (UName "T2") (X_ExtEType (BBoard (Board 1 1 (BInt 1))) (EType [(UName "E2"),(UName "E3")]))))
  ]
  (Board 3 3 (BUName (UName "Space")))
  (Input (BUName (UName "Position")))
  [
    -- initial board
    (ValDef
      (BSig (LName "board") (BBoard (Board 3 3 (BUName (UName "Space")))))
      (BoardEquation (LName "board") (NamePos (LName "x")) (NamePos (LName "x")) (SVal (UName "Empty")))),

    -- next
    (ValDef
      (FSig (LName "next") (FType (BUName (UName "Player")) (BUName (UName "Player"))))
      (FuncEquation (LName "next") [(LName "p")] (Cond
        (BinOp (Ref (LName "p")) (Eq) (SVal (UName "X")))
        (SVal (UName "O"))
        (SVal (UName "X"))))),

    -- factorial, just nestled in here, and unecessary, but let's see if that comes up?
    (ValDef
      (FSig (LName "fact") (FType (BInt x_x) (BInt x_x)))
      (FuncEquation (LName "fact") [(LName "x")] (Cond
        (BinOp (Ref (LName "x")) Greater (IVal 1))
        (BinOp (Ref (LName "x")) (Times)
          (App (LName "fact") [(BinOp (Ref (LName "x")) (Minus) (IVal 1))]))
        (IVal 1)))),

    -- goFirst
    (ValDef
      (BSig (LName "goFirst") (BUName (UName "Player")))
      (ValEquation (LName "goFirst") (SVal (UName "X")))),

    -- outcome
    (ValDef
      (FSig (LName "outcome") (FType
        (BTuple [(BUName (UName "Player")),(BBoard (Board 3 3 (BUName (UName "Space"))))])
        (BUName (UName "Result"))))
      (FuncEquation (LName "outcome") [(LName "p"),(LName "b")] (Cond
        (App (LName "inARow") [(IVal 3),(SVal (UName "S")),(Ref (LName "b"))])
        (SVal (UName "X"))
        (Cond
          (App (LName "inARow") [(IVal 3),(SVal (UName "O")),(Ref (LName "b"))])
          (SVal (UName "O"))
          (SVal (UName "Tie")))))),

    -- threeInARow
    (ValDef
      (FSig (LName "threeInARow") (FType
        (BBoard (Board 3 3 (BUName (UName "Space"))))
        (BUName (UName "Bool"))))
      (FuncEquation (LName "threeInARow") [(LName "b")]
        (App (LName "or") [
          (App (LName "inARow") [(IVal 3),(SVal (UName "X")),(Ref (LName "b"))]),
          (App (LName "inARow") [(IVal 3),(SVal (UName "O")),(Ref (LName "b"))])]))),

    -- gameOver
    (ValDef
      (FSig (LName "gameOver") (FType
        (BBoard (Board 3 3 (BUName (UName "Space"))))
        (BUName (UName "Bool"))))
      (FuncEquation (LName "gameOver") [(LName "b")]
        (App (LName "or") [
          (App (LName "threeInARow") [(Ref (LName "b"))]),
          (App (LName "isFull") [(Ref (LName "b"))])]))),

    --isValid
    (ValDef
      (FSig (LName "isValid") (FType
        (BTuple [(BUName (UName "Player")),(BBoard (Board 3 3 (BUName (UName "Space"))))])
        (BUName (UName "Bool"))))
      (FuncEquation (LName "isValid") [(LName "b"),(LName "p")] (Cond
        (BinOp (BinOp (Ref (LName "b")) (Get) (Ref (LName "p"))) (Eq) (SVal (UName "Empty")))
        (SVal (UName "True"))
        (SVal (UName "False"))))),

    -- tryMove
    (ValDef
      (FSig (LName "tryMove") (FType
        (BTuple [(BUName (UName "Player")),(BBoard (Board 3 3 (BUName (UName "Space"))))])
        (BTuple [(BUName (UName "Player")),(BBoard (Board 3 3 (BUName (UName "Space"))))])))
      (FuncEquation (LName "tryMove") [(LName "p"),(LName "b")]
        (Let (LName "pos") (Ref (LName "input"))
          (Cond
            (BinOp
              (App (LName "isValid") [(Ref (LName "b")),(Ref (LName "pos"))])
              (Eq)
              (SVal (UName "True")))
            (Tup [(App (LName "next") [(Ref (LName "p"))]), (App (LName "place") [(Ref (LName "p")),(Ref (LName "b")),(Ref (LName "pos"))])])
            (Tup [(Ref (LName "p")),(Ref (LName "b"))]))))),

    -- define 'not' in terms of the True/False symbols
    (ValDef
      (FSig (LName "not") (FType
        (BUName (UName "Bool"))
        (BUName (UName "Bool"))))
      (FuncEquation (LName "not") [(LName "y")]
        (Cond
          (BinOp (Ref (LName "y")) (Eq) (SVal (UName "True")))
          (SVal (UName "False"))
          (SVal (UName "False"))))),

    -- loop
    (ValDef
      (FSig (LName "loop") (FType
        (BTuple [(BUName (UName "Player")),(BBoard (Board 3 3 (BUName (UName "Space"))))])
        (BTuple [(BUName (UName "Player")),(BBoard (Board 3 3 (BUName (UName "Space"))))])))
      (FuncEquation (LName "loop") [(LName "p"),(LName "b")]
        (While
          (BinOp
            (App (LName "not")
              [(App (LName "gameOver") [(Ref (LName "b"))])])
            (Eq)
            (SVal (UName "True")))
          (App (LName "tryMove") [(Ref (LName "p")),(Ref (LName "p"))])))),

    -- play
    (ValDef
      (FSig (LName "play") (FType
        (BTuple [(BUName (UName "Player")),(BBoard (Board 3 3 (BUName (UName "Space"))))])
        (BUName (UName "Result"))))
      (FuncEquation (LName "play") [(LName "a"),(LName "b")]
        (App (LName "outcome") [(App (LName "loop") [(Ref (LName "a")),(Ref (LName "b"))])]))),

    -- result
    (ValDef
      (BSig (LName "result") (BUName (UName "Result")))
      (ValEquation (LName "result")
        (App (LName "play") [(Ref (LName "goFirst")), (Ref (LName "initialBoard"))])))
  ])


-- a function that returns the value given as is
p5 :: Game
p5 = (Game
  (UName "P5_Identity")
  []
  (Board 1 1 (BInt x_x))
  (Input (BInt x_x))
  [
    (ValDef
      (FSig (LName "identity") (FType (BInt x_x) (BInt x_x)))
      (FuncEquation (LName "identity") [(LName "x")] ((Ref (LName "x")))))
  ])


progs :: [Game]
progs = [p1,p2,p3,p4,p5]

-- Produces graphs of programs 1 - 4
graphBoglProgs :: IO ()
graphBoglProgs = _graphSimpleProgs (produceGraphs progs) 0

{-
graphEntireLattice :: IO ()
graphEntireLattice = _graphSimpleProgsWithName [(graph_to_concept_graph entireLattice)] 0 "FullLattice"

-- graph of entire lattice
entireLattice :: ([String],[(Dep,String,String)])
entireLattice = ([
  "LName",
  "UName",
  "BType",
  "BInt",
  "BBoard",
  "BUName",
  "BTuple",
  "FType",
  "EType",
  "XType",
  "X_EType",
  "X_ExtEType",
  "X_ExtName",
  "Game",
  "TypeOrValDef",
  "TOV_TypeAssign",
  "TOV_ValDef",
  "Board",
  "Input",
  "TypeAssign",
  "TypeAssignXType",
  "TypeAssignBType",
  "ValDef",
  "Signature",
  "BSig",
  "FSig",
  "Equation",
  "ValEquation",
  "FuncEquation",
  "BoardEquation",
  "Pos",
  "NamePos",
  "IntPos",
  "Expr",
  "IVal",
  "SVal",
  "Ref",
  "Tup",
  "App",
  "BinOp",
  "Let",
  "Cond",
  "While",
  "Plus",
  "Minus",
  "Times",
  "Div",
  "Less",
  "LessEq",
  "Eq",
  "GreaterEq",
  "Greater",
  "NotEq",
  "Get",
  "Int",
  "BBool",
  "Bool",
  "Array",
  "Concept"
  ], [
  ("","LName","Concept"),
  ("","UName","Concept"),
  ("","BType","BInt"),
  ("","BInt","Int"),
  ("","BType","BBool"),
  ("","BBool","Bool"),
  ("","Int","Concept"),
  ("","BType","BInt"),

  ("","BType","BBoard"),
  ("","BBoard","Board"),
  ("","BType","BUName"),
  ("","BUName","UName"),
  ("","BType","BTuple"),
  ("","BTuple","Concept"), -- list of items?

  ("","FType","BType"),

  ("","EType","UName"),

  ("","XType","X_EType"),
  ("","X_EType","EType"),
  ("","XType","X_ExtEType"),
  ("","X_ExtEType","BType"),
  ("","X_ExtEType","EType"),
  ("","XType","X_ExtName"),
  ("","X_ExtName","BType"),
  ("","X_ExtName","UName"),

  ("","Game","UName"),
  ("","Game","TypeOrValDef"),
  ("","Game","Board"),
  ("","Game","Input"),
  ("","Game","ValDef"),

  ("","TypeOrValDef","TOV_TypeAssign"),
  ("","TypeOrValDef","TOV_ValDef"),
  ("","TOV_TypeAssign","TypeAssign"),
  ("","TOV_ValDef","ValDef"),

  ("","Board","Array"),
  ("","Array","Concept"),

  ("","Input","BType"),

  ("","TypeAssign","TypeAssignXType"),
  ("","TypeAssign","TypeAssignBType"),
  ("","TypeAssignXType","UName"),
  ("","TypeAssignXType","XType"),
  ("","TypeAssignBType","UName"),
  ("","TypeAssignBType","BType"),

  ("","ValDef","Signature"),
  ("","ValDef","Equation"),

  ("","Signature","BSig"),
  ("","Signature","FSig"),
  ("","BSig","LName"),
  ("","BSig","BType"),
  ("","FSig","LName"),
  ("","FSig","FType"),

  ("","Equation","ValEquation"),
  ("","Equation","FuncEquation"),
  ("","Equation","BoardEquation"),
  ("","ValEquation","LName"),
  ("","ValEquation","Expr"),
  ("","FuncEquation","LName"),
  ("","FuncEquation","Expr"),
  ("","BoardEquation","LName"),
  ("","BoardEquation","Pos"),
  ("","BoardEquation","Expr"),

  ("","Pos","NamePos"),
  ("","Pos","IntPos"),
  ("","NamePos","LName"),
  ("","IntPos","Int"),

  ("","Expr","IVal"),
  ("","Expr","SVal"),
  ("","Expr","Ref"),
  ("","Expr","Tup"),
  ("","Expr","App"),
  ("","Expr","BinOp"),
  ("","Expr","Let"),
  ("","Expr","Cond"),
  ("","Expr","While"),

  ("","IVal","Int"),
  ("","SVal","UName"),
  ("","Ref","LName"),
  ("","Tup","Concept"),
  ("","App","LName"),
  ("","Let","LName"),
  ("","Cond","Bool"),
  ("","Bool","Concept"),
  ("","While","Concept"),

  ("","BinOp","Plus"),
  ("","BinOp","Minus"),
  ("","BinOp","Times"),
  ("","BinOp","Div"),
  ("","BinOp","Less"),
  ("","BinOp","LessEq"),
  ("","BinOp","Eq"),
  ("","BinOp","GreaterEq"),
  ("","BinOp","Greater"),
  ("","BinOp","NotEq"),
  ("","BinOp","Get"),

  ("","Plus","Concept"),
  ("","Minus","Concept"),
  ("","Times","Concept"),
  ("","Div","Concept"),
  ("","Less","Concept"),
  ("","LessEq","Concept"),
  ("","Eq","Concept"),
  ("","GreaterEq","Concept"),
  ("","Greater","Concept"),
  ("","NotEq","Concept"),
  ("","Get","Concept")

  ])
  -}

-- in order checks
o1 :: [Concept String]
o1 = map (\x -> (Concept x)) ["Concept","Game","UName","Board","Input","BType","[Char]","BInt"]

-- | One for each of the example programs above
knowns :: IO ()
knowns = _knowns 0 o1 (produceGraphs progs)

showDiffs :: [[Concept String]]
showDiffs = produceDiffs o1 (produceGraphs progs)

ordStdDev :: Float
ordStdDev = stdDeviation showDiffs

allStdDevs :: (Data a, Eq a) => [a] -> (Float,[a])
allStdDevs pgs =  let f = (produceDiffs o1) . produceGraphs in
                    let progPermutations = permutations pgs in
                    let stdDevs = map (stdDeviation . f) progPermutations in
                    let m = minimum stdDevs in
                    let i = elemIndex m stdDevs in
                    case i of
                      Just index -> (m,progPermutations !! index)
                      Nothing    -> (m,[])

sortList :: (Data a, Eq a) => [a] -> (a -> String) -> (Float,[String])
sortList pl f = let rr = allStdDevs pl in
              (fst rr, map f (snd $ rr))

gameName :: Game -> String
gameName (Game (UName n) _ _ _ _) = n

sortList1 :: (Float,[String])
sortList1 = let pl = [p1,p2,p5,p3,p4] in
            sortList pl gameName

sortList2 :: (Float,[String])
sortList2 = let pl = [p3,p4,p5,p1,p2] in
            sortList pl gameName
