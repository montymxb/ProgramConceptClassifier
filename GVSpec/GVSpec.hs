--
-- GVSpec.hs
--
-- Handles produces Graph Viz files from various types
--

module GVSpec(GVData,convertToGVSpec,writeGVSpec) where

import Symbol
import Concept
import ConceptDependency
import GrammarToGraph
import ConceptGraph
import System.Process

type GVData = ([String],[(String,String)])

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
              _   -> (c:cn ls)

rstrip :: String -> String
rstrip x = case reverse x of
            ('\n':ls) -> reverse ls
            _         -> x


printGVSpec :: GVData -> String
printGVSpec ([],[]) = "\n}"
printGVSpec ([], ((s1,s2):ls2)) = let r1 = cn s1 in
                                  let r2 = cn s2 in
                                  r1 ++ " -> " ++ r2 ++ ";\n" ++ printGVSpec ([],ls2)
printGVSpec ((s:ls1), ls2) = let sr = rstrip s in
                             let rr = cn sr in
                             rr ++ " [label=\"" ++ sr ++ "\"];\n" ++ printGVSpec (ls1,ls2)

convertToGVSpec :: (Show a, Show b) => ConceptGraph a b -> String
convertToGVSpec (ConceptGraph vertices edges) = "digraph G {\n" ++ printGVSpec (map show vertices,map (\(ConceptDependency _ n1 n2) -> (show n1, show n2)) edges)

writeGVSpec :: (Show a, Show b) => String -> ConceptGraph a b -> IO ()
writeGVSpec name cg = do
  writeFile (name ++ ".gv") $ convertToGVSpec cg
  system ("dot -Tpng -o"++ name ++".png " ++ name ++".gv")
  return ()
