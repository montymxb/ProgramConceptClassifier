--
-- GVSpec.hs
--
-- Handles produces Graph Viz files from various types
--

module GVSpec.GVSpec (GVData,convertToGVSpec,writeGVSpec,writeLattice,ShowGV,showGV) where

import ConceptGraph.ConceptGraph
import System.Process
import Data.List

type EdgeData = String
type Node1Data = String
type Node2Data = String

type UID = String
type Label = String

type GVData = ([String],[(EdgeData,Node1Data,Node2Data)])

type GraphVizData = ([(UID,Label)],[(UID,UID)])

-- class for showing the GV Spec of an instance of 'a'
class ShowGV a where
  showGV :: a -> String

-- | Convert names of symbols into safer characters
cn :: String -> String
cn [] = []
cn (c:ls) = case c of
              '!' -> ("Exclaim"++(cn ls))
              '&' -> ("And"++(cn ls))
              '(' -> ("OParen"++(cn ls))
              ')' -> ("CParen"++(cn ls))
              '{' -> ("OCurly"++(cn ls))
              '}' -> ("CCurly"++(cn ls))
              '=' -> ("Equal"++(cn ls))
              ':' -> ("Colon"++(cn ls))
              '-' -> ("Minuss"++(cn ls))
              '>' -> ("Gt"++(cn ls))
              '*' -> ("Timess"++(cn ls))
              '+' -> ("Pluss"++(cn ls))
              '%' -> ("Modd"++(cn ls))
              '/' -> ("Divi"++(cn ls))
              '\"'-> (cn ls)
              ' ' -> ("_"++(cn ls))
              '[' -> ("OS" ++ cn ls) -- used to factor in the list, now we will ignore it!
              ']' -> ("CS" ++ cn ls)
              ',' -> ("Comma" ++ (cn ls))
              _   -> (c:cn ls)

rstrip :: String -> String
rstrip x = case reverse x of
            ('\n':ls) -> reverse ls
            _         -> x

qtrim :: String -> String
qtrim x = case x of
            ('"':ls) -> case reverse ls of
                          ('"':ls2) -> reverse ls2
                          _         -> ls
            _        -> x

printGVSpec :: GVData -> String
printGVSpec ([],[]) = "\n}"
-- TODO use this edge 'ee' to add some extra data to the edge type?
-- print edges
printGVSpec ([], ((ee,s1,s2):ls2)) = let r1 = cn s1 in
                                  let r2 = cn s2 in
                                  r1 ++ "\t->\t" ++ r2 ++ "\t" ++ printColor ee ++ "\n" ++ printGVSpec ([],ls2) -- ee ++ ";\n"
-- print vertices
printGVSpec ((s:ls1), ls2) = let sr = rstrip s in
                             let rr = cn sr in
                             rr ++ "\t" ++ printLabel sr ++ "\n" ++ printGVSpec (ls1,ls2)


-- | Used for printing lattice related data
printLatticeGVData :: GraphVizData -> String
printLatticeGVData ([],[]) = "\n}"
-- print edges
printLatticeGVData ([], ((s1,s2):ls2)) = let r1 = cn s1 in
                                            let r2 = cn s2 in
                                            --let sr = rstrip lb in
                                            r1 ++ "\t->\t" ++ r2 ++ "\n" ++ printLatticeGVData ([],ls2) -- ee ++ ";\n"
-- print vertices
printLatticeGVData (((uid,s):ls1), ls2) = let sr = rstrip s in
                             let rr = cn sr in
                             uid ++ "\t" ++ printLabel sr ++ "\n" ++ printLatticeGVData (ls1,ls2)

printLabel :: String -> String -- "black:invis:black"
printLabel l = " [label=\"" ++ qtrim(rstrip l) ++ "\"];"


printColor :: String -> String -- "black:invis:black"
printColor l = " [color=\"" ++ qtrim(rstrip l) ++ "\"];"


convertToGVSpec :: (Show a, Show b) => ConceptGraph a b -> String
-- TODO use the '_' (the dep type) to decide how to graph the edge...
convertToGVSpec (ConceptGraph vertices edges) = "strict digraph G {\n" ++ printGVSpec (map show vertices,map (\(ee,n1,n2) -> (show ee, show n1, show n2)) edges)


uidForVerts :: Int -> [a] -> [(UID,a)]
uidForVerts _ [] = []
uidForVerts x (y:ys) = (("V" ++ (show x), y) : (uidForVerts (x+1) ys))


uidForEdges :: Eq a => [(UID,a)] -> [(a,a)] -> [(UID,UID)]
uidForEdges verts edges = let firsts = map (\(a,b) -> case (find (\(_,n) -> a == n) verts) of
                                                        Just (u,_) -> (u,b)
                                                        Nothing-> error "first FAILED") edges in
                         let scnds  = map (\(a,b) -> case (find (\(_,n) -> b == n) verts) of
                                                        Just (u,_) -> (a,u)
                                                        Nothing-> error "second FAILED") firsts in
                         scnds


convertToGVData :: Eq a => ([a],[(a,a)]) -> ([(UID,a)],[(UID,UID)])
convertToGVData (verts,edges) = let v2 = uidForVerts 1 verts in -- first convert all verts
                                let e2 = uidForEdges v2 edges in-- then map & replace all edges accordingly
                                (v2,e2)


-- | Convert Lattice to a Graph Viz Specification
convertLatticeToGVSpec :: (ShowGV a, Eq a) => ([a],[(a,a)]) -> String
convertLatticeToGVSpec (vertices,edges) = let (gVerts,gEdges) = convertToGVData (vertices,edges) in
                                          "strict digraph G {\n" ++ printLatticeGVData (map (\(a,b) -> (a,showGV b)) gVerts,map (\(n1,n2) -> (show n1, show n2)) gEdges)


-- | Used for graphing a ConceptGraph (old)
writeGVSpec :: (Show a, Show b) => String -> ConceptGraph a b -> IO ()
writeGVSpec name cg = do
  writeFile (name ++ ".gv") $ convertToGVSpec cg
  _ <- system ("dot -Tpng -o"++ name ++".png " ++ name ++".gv")
  return ()


-- | Used for graphing a Concept Lattice
writeLattice :: (ShowGV a, Eq a) => String -> ([a],[(a,a)]) -> IO ()
writeLattice name cl = do
  writeFile (name ++ ".gv") $ convertLatticeToGVSpec cl
  _ <- system ("dot -Tpng -o"++ name ++".png " ++ name ++".gv")
  return ()
