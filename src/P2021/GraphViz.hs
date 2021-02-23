{-# LANGUAGE ConstrainedClassMethods #-}

module P2021.GraphViz where

import System.Process
import Data.List
import P2021.General

type UID = String

uidForVerts :: GraphVizable a => Int -> [a] -> [(UID,a)]
uidForVerts _ [] = []
uidForVerts x (y:ys) = (("V" ++ (show x), y) : (uidForVerts (x+1) ys))


uidForEdges :: (Eq a, GraphVizable a) => [(UID,a)] -> [(a,a)] -> [((UID,a),(UID,a))]
uidForEdges verts edges = let firsts = map (\(a,b) -> case (find (\(_,n) -> a == n) verts) of
                                                        Just (u,_) -> ((u,a),b)
                                                        Nothing-> error "first FAILED") edges in
                          let scnds  = map (\(a,b) -> case (find (\(_,n) -> b == n) verts) of
                                                        Just (u,_) -> (a,(u,b))
                                                        Nothing-> error "second FAILED") firsts in
                          scnds

foldPairs :: [(String,String)] -> String
foldPairs ls = foldl (\s (a,b) -> "[" ++ a ++ "=\"" ++ b ++ "\"]" ++ s) "" ls

class GraphVizable a where
  -- node properties
  node :: a -> [(String,String)]
  -- edge properties
  edge :: a -> a -> [(String,String)]

  -- construct a digraph using the dot program
  makeDGraph :: Eq a => String -> ([a],[(a,a)]) -> IO ()
  makeDGraph name (n,e) = do
    makeDot name (n,e)
    _ <- system ("dot -Tpng -o"++ name ++".png " ++ name ++".gv")
    return ()

  -- writes out a DOT file suitable for using with `dot`
  makeDot :: Eq a => String -> ([a],[(a,a)]) -> IO ()
  makeDot name (n,e) = do
    let uV = uidForVerts 1 n
    let uE = uidForEdges uV e
    let nodes = map (\(u,x) -> u ++ " " ++ foldPairs (node x)) uV
    let edges = map (\((u,a),(u',b)) -> u ++ " -> " ++ u' ++ " " ++ foldPairs (edge a b)) uE
    writeFile (name ++ ".gv") $ "strict digraph {\n" ++ join ";\n" nodes ++ ";\n" ++ join ";\n" edges ++ "\n}"
