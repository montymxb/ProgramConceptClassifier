{-# LANGUAGE ConstrainedClassMethods #-}

module GraphViz where

import System.Process
import Data.List
import General

-- | Universally Unique Identifier, used to describe elements independent of their contents
type UID = String

-- | Produces uniquely identifiable vertices
uidForVerts :: GraphVizable a => Int -> [a] -> [(UID,a)]
uidForVerts _ [] = []
uidForVerts x (y:ys) = (("V" ++ (show x), y) : (uidForVerts (x+1) ys))


-- | Produces uniquely identifiable edges
uidForEdges :: (Eq a, GraphVizable a) => [(UID,a)] -> [(a,a)] -> [((UID,a),(UID,a))]
uidForEdges verts edges = let firsts = map (\(a,b) -> case (find (\(_,n) -> a == n) verts) of
                                                        Just (u,_) -> ((u,a),b)
                                                        Nothing-> error "first FAILED") edges in
                          let scnds  = map (\(a,b) -> case (find (\(_,n) -> b == n) verts) of
                                                        Just (u,_) -> (a,(u,b))
                                                        Nothing-> error "second FAILED") firsts in
                          scnds

-- | Folds pairs of strings into GV properties
foldPairsIntoProps :: [(String,String)] -> String
foldPairsIntoProps ls = foldl (\s (a,b) -> "[" ++ a ++ "=\"" ++ b ++ "\"]" ++ s) "" ls

-- | Typeclass for objects that can be used to generate DOT specifications for GV graphs
class GraphVizable a where
  -- | Generate key:value pairs for node properties
  node :: a -> [(String,String)]

  -- | Generate key:value pairs for edge properties
  edge :: a -> a -> [(String,String)]

  -- | Construct a digraph using a DOT program
  makeDGraph :: Eq a => String -> ([a],[(a,a)]) -> IO ()
  makeDGraph name (n,e) = do
    makeDot name (n,e)
    _ <- system ("dot -Tpng -o"++ name ++".png " ++ name ++".gv")
    return ()

  -- | Writes out a DOT file suitable for using with `dot`, system dependent
  makeDot :: Eq a => String -> ([a],[(a,a)]) -> IO ()
  makeDot name (n,e) = do
    let uV = uidForVerts 1 n
    let uE = uidForEdges uV e
    let nodes = map (\(u,x) -> u ++ " " ++ foldPairsIntoProps (node x)) uV
    let edges = map (\((u,a),(u',b)) -> u ++ " -> " ++ u' ++ " " ++ foldPairsIntoProps (edge a b)) uE
    writeFile (name ++ ".gv") $ "strict digraph {\n" ++ join ";\n" nodes ++ ";\n" ++ join ";\n" edges ++ "\n}"
