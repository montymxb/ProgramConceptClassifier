--
-- GVSpec.hs
--
-- Handles produces Graph Viz files from various types
--

module GVSpec.GVSpec (GVData,convertToGVSpec,writeGVSpec,ShowGV) where

import Grammar.Symbol
import ConceptGraph.Concept
import ConceptGraph.ConceptDependency
import Grammar.GrammarToGraph
import ConceptGraph.ConceptGraph
import System.Process

type EdgeData = String
type Node1Data = String
type Node2Data = String

type GVData = ([String],[(EdgeData,Node1Data,Node2Data)])

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
              '[' -> (cn ls) -- used to factor in the list, now we will ignore it!
              ']' -> (cn ls)
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
                                  r1 ++ "\t->\t" ++ r2 ++ "\t" ++ printLabel ee ++ "\n" ++ printGVSpec ([],ls2) -- ee ++ ";\n"
-- print vertices
printGVSpec ((s:ls1), ls2) = let sr = rstrip s in
                             let rr = cn sr in
                             rr ++ "\t" ++ printLabel sr ++ "\n" ++ printGVSpec (ls1,ls2)


printLabel :: String -> String
printLabel l = " [label=\"" ++ qtrim(rstrip l) ++ "\"];"

convertToGVSpec :: (Show a, Show b) => ConceptGraph a b -> String
-- TODO use the '_' (the dep type) to decide how to graph the edge...
convertToGVSpec (ConceptGraph vertices edges) = "strict digraph G {\n" ++ printGVSpec (map show vertices,map (\(ee,n1,n2) -> (show ee, show n1, show n2)) edges)

writeGVSpec :: (Show a, Show b) => String -> ConceptGraph a b -> IO ()
writeGVSpec name cg = do
  writeFile (name ++ ".gv") $ convertToGVSpec cg
  system ("dot -Tpng -o"++ name ++".png " ++ name ++".gv")
  return ()
