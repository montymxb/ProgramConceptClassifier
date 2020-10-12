{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ConstrainedClassMethods #-}
--
-- BOGL_P1
--

module AST.BOGL_AST where

import Grammar.Grammar
import ConceptGraph.GraphToConceptGraph
import ConceptGraph.ConceptGraph
import Query.Query
import GVSpec.GVSpec as GVSpec
import Data.Data
import Data.Generics
import Unsafe.Coerce
import qualified Data.Set as S
import Data.Set (toList,fromList)
import Data.List

import Debug.Trace

import ConceptGraph.Concept
import ConceptGraph.ConceptDependency

import Data.Generics.Uniplate.Data

import AbstractSyntax.BOGL

-- Don't Care Value
x_x :: Int
x_x = 101

-- p1, simplest game
p1 :: Game
p1 = (Game
  (UName "P1")
  []
  (Board 1 1 (BInt 1))
  (Input (BInt x_x))
  [])


-- p2, game with type and function that returns an Int
p2 :: Game
p2 = (Game
  (UName "P2")
  [
    (TOV_TypeAssign (TypeAssignXType (UName "T1") (X_EType (EType [(UName "E1")])))),
    (TOV_ValDef (ValDef (BSig (LName "f1") (BInt x_x)) (ValEquation (LName "f1") (IVal x_x))))
  ]
  (Board 1 1 (BInt 1))
  (Input (BInt x_x))
  [])



-- practical examples


-- factorial
p3 :: Game
p3 =
  (Game
  (UName "Factorial_Example")
  [
    (TOV_TypeAssign (TypeAssignXType (UName "T1") (X_EType (EType [(UName "E1")])))),
    (TOV_TypeAssign (TypeAssignXType (UName "T2") (X_ExtEType (BBoard (Board 1 1 (BInt 1))) (EType [(UName "E2"),(UName "E3")]))))
  ]
  (Board 1 1 (BInt 1))
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


--
-- Going to just setup a conceptual typeclass, that works like show, but just gives details about the concepts, and their relationships
--
toConstr' x = show (toConstr x)

-- | Removes duplicates from a list, whilst preserving order
_makeUnique :: (Eq a) => [a] -> [a]
_makeUnique [] = []
_makeUnique (x:ls) | elem x ls = _makeUnique ls -- drop, already present
                   | otherwise = x : _makeUnique ls-- keep it

makeUnique :: (Eq a) => [a] -> [a]
makeUnique = reverse . _makeUnique . reverse


getAllEdgesFor :: (Eq a,Eq b) => a -> [(b,a,a)] -> [(b,a,a)]
getAllEdgesFor n ls = filter (\(_,from,_) -> from == n) ls

identifyCycles :: (Eq a, Eq b) => [a] -> [(b,a,a)] -> [(b,a,a)]
identifyCycles visited edges = filter (\(_,_,x) -> elem x visited) edges

findCycles :: (Eq a, Eq b) => a -> [a] -> [(b,a,a)] -> [(b,a,a)]
findCycles _ _ [] = []
findCycles n visited ls = let edges = getAllEdgesFor n ls in           -- get all edges that start w/ 'n'
                                     let cycles = identifyCycles visited edges in -- foreach edge, if the 'to' element is in visited, it forms a cycle, skip it and return that cycle
                                     let goodEdges = edges \\ cycles in           -- get the good edges that do not form cycles
                                     let results = concatMap (\(_,_,b) -> findCycles b (n:visited) ls) goodEdges in -- reapply this check from all of those edges that do not form cycles
                                     cycles ++ results -- returns the results of this check and any other checks combined
                                     -- stops when there are no more edges to traverse (should end eventually)

-- remove cycles
removeCycles :: (Eq a, Eq b) => a -> [(b,a,a)] -> [(b,a,a)]
removeCycles _ [] = []
removeCycles n lsa@(s@(_,a,c):ls) = filter (\x -> not (elem x cycleEdges)) lsa where  -- only returns edges that do not form cycles
  cycleEdges = findCycles n [] lsa

unbox :: String -> String
unbox [] = []
unbox ('[':x) = if reverse x !! 0 == ']' then init x else x
unbox x  = x

quote :: String -> String
quote s = '\'' : s ++ "'"

-- Verifies every edge ends leads to Concept
verifyLattice :: (Show b,Eq b) => [(b,String,String)] -> [(b,String,String)] -> (Bool,String)
verifyLattice [] _ = (True,"")
verifyLattice (x@(_,a,b):ls) ls2 | b /= "Concept" = let edges = getAllEdgesFor (unbox b) ls2 in
                               case edges of
                                 [] -> (False,"\n\nIn Lattice, " ++ quote b ++ " does not connect to any other node. It should be pointing to 'Concept' to complete the lattice.\n\nThis can be fixed by adding 'ebase s', where 's' is the element in question, and append this to the existing edge list.\n\n")
                                 els-> verifyLattice ls ls2 -- continue
                                 | otherwise =  verifyLattice ls ls2-- skip to the next entry, if any

type EdgeName = String
type FromNode = String
type ToNode = String
type CNodes = [String]
type CEdges = [(EdgeName,FromNode,ToNode)]
class Conceptual a where
  cgraph :: (Typeable a, Data a, Eq a) => a -> (CNodes,CEdges)
  cgraph x = let uniqueConcepts = (makeUnique . concepts) x in
             let uniqueEdges = (makeUnique . _edges) x in
             let reducedEdges = foldl (\tot x -> removeCycles x tot) uniqueEdges uniqueConcepts in
             let verifyResult = verifyLattice reducedEdges reducedEdges in
             case (verifyResult) of
               (True,_)    -> (uniqueConcepts, reducedEdges)
               (False,msg) -> error ("Unable to verify lattice with error: " ++ msg)

  concepts :: (Typeable a, Data a, Eq a) => a -> CNodes
  concepts c = [show (typeOf c), toConstr' c, "Concept"]

  _edges :: (Typeable a, Data a, Eq a) => a -> CEdges
  -- tie 'a' from type -> constr name -> Concept, assuming this is a base type
  _edges x = [("",show (typeOf x), toConstr' x),("",toConstr' x, "Concept")]

  -- Builds an edge from X -> Y
  getEdges :: (Data a, Typeable a) => String -> a -> [(EdgeName,FromNode,ToNode)]
  getEdges cname a = [("",cname,show (typeOf a))]



instance (Data a, Eq a, Conceptual a) => Conceptual [a] where
  concepts x = concatMap concepts x

  _edges []   = []
  _edges ls   = concatMap _edges ls

  getEdges cname [] = []
  getEdges cname ls = map (\y -> ("",cname,show (typeOf y))) (ls)


-- Self edge
selfEdge :: (Data a, Typeable a) => a -> (EdgeName,FromNode,ToNode)
selfEdge x = ("",show (typeOf x), toConstr' x)

instance Conceptual Int where
  concepts x = ["Int","Concept"]
  _edges x = [("","Int","Concept")]

instance Conceptual Double where
  concepts x = ["Double","Concept"]
  _edges x = [("", "Double", "Concept")]

instance Conceptual Float where
  concepts x = ["Float","Concept"]
  _edges x = [("", "Float", "Concept")]

instance Conceptual Word where
  concepts x = ["Word","Concept"]
  _edges x = [("", "Word", "Concept")]

instance Conceptual Bool where
  concepts x = ["Bool","Concept"]
  _edges x = [("", "Bool", "Concept")]

instance Conceptual Char where
  concepts x = ["Char","Concept"]
  _edges x = [("", "Char", "Concept")]

ccs :: (Typeable a, Data a) => a -> CNodes
ccs s = [(show (typeOf s)),toConstr' s]

c0 :: (Data a, Typeable a) => a -> CNodes
c0 a = ccs a

-- base concept
cbase :: (Data a, Typeable a) => a -> CNodes
cbase a = [toConstr' a, "Concept"]

cb :: (Data b, Typeable b, Conceptual b, Eq b) => b -> CNodes
cb = concepts

c1 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b) => a -> b -> CNodes
c1 a b = c0 a ++ cb b

c2 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c) => a -> b -> c -> CNodes
c2 a b c = c0 a ++ cb b ++ cb c

c3 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d) => a -> b -> c -> d -> CNodes
c3 a b c d = c0 a ++ cb b ++ cb c ++ cb d

c4 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d, Eq e, Data e, Typeable e, Conceptual e) => a -> b -> c -> d -> e -> CNodes
c4 a b c d e = c0 a ++ cb b ++ cb c ++ cb d ++ cb e

c5 :: (Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d, Eq e, Data e, Typeable e, Conceptual e, Eq f, Data f, Typeable f, Conceptual f) => a -> b -> c -> d -> e -> f -> CNodes
c5 a b c d e f = c0 a ++ cb b ++ cb c ++ cb d ++ cb e ++ cb f

-- basing edge
ebase :: (Data a, Typeable a) => a -> CEdges
ebase a = [("",show (typeOf a),toConstr' a),("",toConstr' a,"Concept")]

e0 :: (Data a, Typeable a) => a -> CEdges
e0 a = [selfEdge a]

e1 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b) => a -> b -> CEdges
e1 a b = [selfEdge a] ++ (getEdges cname b) ++ _edges b where
  cname = toConstr' a

e2 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c) => a -> b -> c -> CEdges
e2 a b c = [selfEdge a] ++ (getEdges cname b) ++ (getEdges cname c) ++ _edges b ++ _edges c where
  cname = toConstr' a

e3 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d) => a -> b -> c -> d -> CEdges
e3 a b c d = [selfEdge a] ++ (getEdges cname b) ++ (getEdges cname c) ++ (getEdges cname d) ++ _edges b ++ _edges c ++ _edges d where
  cname = toConstr' a

e4 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d, Eq e, Data e, Typeable e, Conceptual e) => a -> b -> c -> d -> e -> CEdges
e4 a b c d e = [selfEdge a] ++ (getEdges cname b) ++ (getEdges cname c) ++ (getEdges cname d) ++ (getEdges cname e) ++ _edges b ++ _edges c ++ _edges d ++ _edges e where
  cname = toConstr' a

e5 :: (Conceptual a, Data a, Typeable a, Data b, Typeable b, Conceptual b, Eq b, Data c, Typeable c, Eq c, Conceptual c, Eq d, Data d, Typeable d, Conceptual d, Eq e, Data e, Typeable e, Conceptual e, Eq f, Data f, Typeable f, Conceptual f) => a -> b -> c -> d -> e -> f -> CEdges
e5 a b c d e f = [selfEdge a] ++ (getEdges cname b) ++ (getEdges cname c) ++ (getEdges cname d) ++ (getEdges cname e) ++ (getEdges cname f) ++ _edges b ++ _edges c ++ _edges d ++ _edges e ++ _edges f where
  cname = toConstr' a


instance Conceptual LName where
  concepts s@(LName a) = c1 s a
  _edges s@(LName a)   = e1 s [a]

instance Conceptual UName where
  concepts s@(UName a) = c1 s a
  _edges s@(UName a)   = e1 s [a]


instance Conceptual BType where
  concepts s@(BInt a) = c1 s a
  concepts s@(BBoard a) = c1 s a
  concepts s@(BUName a) = c1 s a
  concepts s@(BTuple a) = c1 s a
  _edges s@(BInt a)     = e1 s [a]
  _edges s@(BBoard a)   = e1 s [a] ++ ebase s
  _edges s@(BUName a)   = e1 s [a]
  _edges s@(BTuple ls)  = e1 s ls ++ ebase s


instance Conceptual FType where
  concepts s@(FType a b) = c2 s a b
  _edges s@(FType a b)   = e2 s [a] [b]

instance Conceptual EType where
  concepts s@(EType ls) = c1 s ls
  _edges s@(EType ls) = e1 s ls


instance Conceptual XType where
  concepts s@(X_EType a) = c1 s a
  concepts s@(X_ExtEType a b) = c2 s a b
  concepts s@(X_ExtName a b) = c2 s a b
  _edges s@(X_EType a) = e1 s [a]
  _edges s@(X_ExtEType a b) = e2 s [a] [b]
  _edges s@(X_ExtName a b) = e2 s [a] [b]


instance Conceptual Game where
  concepts s@(Game a ls b c ls2) = c5 s a ls b c ls2
  _edges s@(Game a ls b c ls2) = e5 s [a] ls [b] [c] ls2

instance Conceptual TypeOrValDef where
  concepts s@(TOV_TypeAssign a) = c1 s a
  concepts s@(TOV_ValDef a) = c1 s a
  _edges s@(TOV_TypeAssign a) = e1 s [a]
  _edges s@(TOV_ValDef a) = e1 s [a]

instance Conceptual Board where
  concepts s@(Board a b c) = c3 s a b c
  _edges s@(Board a b c) = e3 s [a] [b] [c]

instance Conceptual Input where
  concepts s@(Input a) = c1 s a
  _edges s@(Input a) = e1 s [a]

instance Conceptual TypeAssign where
  concepts s@(TypeAssignXType a b) = c2 s a b
  concepts s@(TypeAssignBType a b) = c2 s a b
  _edges s@(TypeAssignXType a b) = e2 s [a] [b]
  _edges s@(TypeAssignBType a b) = e2 s [a] [b]

instance Conceptual ValDef where
  concepts s@(ValDef a b) = c2 s a b
  _edges s@(ValDef a b) = e2 s [a] [b]

instance Conceptual Signature where
  concepts s@(BSig a b) = c2 s a b
  concepts s@(FSig a b) = c2 s a b
  _edges s@(BSig a b) = e2 s [a] [b]
  _edges s@(FSig a b) = e2 s [a] [b]

instance Conceptual Equation where
  concepts s@(ValEquation a b) = c2 s a b
  concepts s@(FuncEquation a b c) = c3 s a b c
  concepts s@(BoardEquation a b c d) = c4 s a b c d
  _edges s@(ValEquation a b) = e2 s [a] [b]
  _edges s@(FuncEquation a b c) = e3 s [a] b [c]
  _edges s@(BoardEquation a b c d) = e4 s [a] [b] [c] [d]

instance Conceptual Pos where
  concepts s@(NamePos a) = c1 s a
  concepts s@(IntPos a) = c1 s a
  _edges s@(NamePos a) = e1 s [a]
  _edges s@(IntPos a) = e1 s [a]

instance Conceptual Expr where
  concepts s@(IVal a) = c1 s a
  concepts s@(SVal a) = c1 s a
  concepts s@(Ref a) = c1 s a
  concepts s@(Tup a) = c1 s a
  concepts s@(App a b) = c2 s a b
  concepts s@(BinOp a b c) = c3 s a b c
  concepts s@(Let a b c) = c3 s a b c
  concepts s@(Cond a b c) = c3 s a b c
  concepts s@(While a b) = c2 s a b
  _edges s@(IVal a) = e1 s [a]
  _edges s@(SVal a) = e1 s [a]
  _edges s@(Ref a) = e1 s [a]
  _edges s@(Tup a) = e1 s a ++ ebase s
  _edges s@(App a b) = e2 s [a] b
  _edges s@(BinOp a b c) = e3 s [a] [b] [c]
  _edges s@(Let a b c) = e3 s [a] [b] [c]
  _edges s@(Cond a b c) = e3 s [a] [b] [c] ++ ebase s
  _edges s@(While a b) = e2 s [a] [b] ++ ebase s

instance Conceptual BinOp where
  concepts s = cbase s
  _edges s = ebase s

-- concept graph in pure BoGL
-- GVSpec.writeGVSpec "astgraph2" boglGraph2
boglGraph2_p1 :: ConceptGraph String String
boglGraph2_p1 = graph_to_concept_graph $ cgraph p1

boglGraph2_p2 :: ConceptGraph String String
boglGraph2_p2 = graph_to_concept_graph $ cgraph p2

boglGraph2_p3 :: ConceptGraph String String
boglGraph2_p3 = graph_to_concept_graph $ cgraph p3

boglGraph2_p4 :: ConceptGraph String String
boglGraph2_p4 = graph_to_concept_graph $ cgraph p4

-- Produces graphs of programs 1 - 4
graphBOGL2 :: IO ()
graphBOGL2 = do
  GVSpec.writeGVSpec "bogl4_p1" boglGraph2_p1
  GVSpec.writeGVSpec "bogl4_p2" boglGraph2_p2
  GVSpec.writeGVSpec "bogl4_p3" boglGraph2_p3
  GVSpec.writeGVSpec "bogl4_p4" boglGraph2_p4
  return ()



--
-- Order Checking
--
type Known a = [Concept a]

-- takes a concept, list of deps, and produces concepts we are dependent on, and those dependent concepts and so forth
deps :: (Conceptual a, Eq a) => Concept a -> Known a -> [ConceptDependency b a] -> [Concept a]
deps _ _ []     = []
deps c@(Concept c1) k ((_,frm,to):ls) | c1 == frm && not (elem (Concept to) k) = ((Concept to) : deps (Concept to) k ls) ++ deps c k ls -- only add deps that we don't already know
                                    | otherwise = deps c k ls

-- represents an order check result
data KnownCheck = OK
  | Unknown String
  deriving (Show)

unconcept :: Concept a -> a
unconcept (Concept a) = a

isKnown :: Known String -> ConceptGraph b String -> KnownCheck
isKnown _ (ConceptGraph [] _)        = OK  -- nothing is required to understand nothing
isKnown [] (ConceptGraph _ _)        = Unknown "If nothing is known, then nothing can be known besides nothing"
isKnown k (ConceptGraph (a:ls) cds)  = let d = makeUnique (deps a k cds) in -- get deps
                                         let knownChecks = map (\x -> elem x k) d in -- check which deps we know
                                         let allKnown = all (\x -> x) knownChecks in -- verify we know all the deps
                                         case allKnown of
                                           True   -> isKnown (a:k) (ConceptGraph ls cds) -- check the next concept, adding 'a' to the known list
                                           False  -> Unknown $ "Concept " ++ (quote . unconcept) a ++ " was determined to be unknown with regards to deps:\n" ++ intercalate "\n" (map unconcept ((filter (\x -> not(elem x k)) d)))

-- in order checks
o1 :: [Concept String]
o1 = map (\x -> (Concept x)) ["Concept","Game","UName","Board","Int","Input","BType","[Char]","BInt"]

showRez (Unknown s) = putStrLn $ "\n\n" ++ s ++ "\n\n"
showRez (OK) = putStrLn "\n\nAll concepts known and OK\n\n"

known1 = showRez (isKnown o1 boglGraph2_p1)
known2 = showRez (isKnown o1 boglGraph2_p2)
known3 = showRez (isKnown o1 boglGraph2_p3)
known4 = showRez (isKnown o1 boglGraph2_p4)
