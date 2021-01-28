module Dec_2020_Old_Main where

import Programs.BOGL_R1
import ConceptGraph.Conceptual
import ConceptGraph.ConceptGraph
import ConceptGraph.ManualConcepts as MC
import Data.Data
import Data.List

-- BoGL Components
import Language.Syntax (Game)
import Text.Parsec.Pos

import qualified Data.Set as DS

import qualified DB.BoglDB as BDB

uniqueList :: Ord a => [a] -> [a]
uniqueList ls = DS.toList $ DS.fromList ls

main_old :: IO [[Concept]]
main_old = do
  -- [ Step 1 ] Turns 2 programs into concept lists
  -- process known prog
  let knownProg = step3 $ step2 $ step1 boglProg2
  -- process unknown prog
  let unknownProg = step3 $ step2 $ step1 boglProg

  -- [ Step 2 ] Compute concept paths, using the deps in our ground truth, for the unknown concepts,
  -- and filter out known concepts, empty paths, redundant paths, and redundant concepts
  -- compute paths for all concepts in the unknownProg
  let pths = concat $ map (\x -> paths (Single x)) unknownProg
  -- filter out known items by list diff
  let fp1 = map (\x -> x \\ knownProg) pths -- TODO [THESIS], more passive understanding, state transitivity of knowledge in thesis as well
  -- filter out empty paths
  let fp2 = filter (\x -> length x > 0) fp1
  -- filter redandant paths
  let fp3 = uniqueList fp2
  -- filter redundant concepts from individual paths
  let fp4 = map uniqueList fp3

  -- [Step 3] Apply Constraints on the paths to require inclusion/exclusion of concepts
  -- inclusion
  let fp5 = filter (\x -> any (\y -> y `elem` [FunctionSignature]) x) fp4
  -- exclusion
  let fp6 = filter (\x -> not $ any (\y -> y `elem` [Board,Input]) x) fp5

  -- TODO [Step 4] Apply Constraints to order paths
  -- Potentially by size (number of unknown concepts at this point)
  --let fp7 = sort fp6

  -- TODO ordering by complex measure
  -- TODO extend known prog to [prog]
  -- TODO complexity is size, loc, # of concepts, # of progs, total size...

  -- [Step 5] For each concept in each path, return a list of programs that could be used to explain it

  -- Map each concept list to a program set that best fits it
  -- the 'best fit' part is based on some criterion
  -- one I believe would work is to pick the set of programs that introduces each concept (in order), with the fewest number of unknown/new concepts
  -- but we can show for each concept a set of programs that can be used to explain a concept
  let explainingProgs = map (\x -> map (\y -> step5 y db_examples) x) fp6
  putStrLn $ show explainingProgs
  return fp6

-- Running this should do the following
-- 1. Read a fixed bogl program (real1), for now, we can change this later
step1 :: Game SourcePos -> ConceptGraph Dep String
step1 = conceptGraph

-- 2. Using a generic map, extract textual representations of the concepts, produces a [String]
--      we could do the CG, but we would have to go back to reconvert the concepts and deps, so avoid it for now
step2 :: ConceptGraph Dep String -> [String]
step2 cg = concepts cg

-- 3. Map raw concepts to manually provided concepts, not a 1-1 mapping, some won't encode, and some may encode to multiple concepts
--      NOT all data types & constructors express into manual concepts, and some may express into multiple
encodeConcept :: String -> [Concept]
encodeConcept "Game" = [Game]
encodeConcept "InputDef" = [Input]
encodeConcept "BVal" = [BoardDef,BoardSignature,BoardEquation]
encodeConcept "Val" = [ValueDef,ValueSignature,ValueEquation]
encodeConcept "Type" = [Type,TypeDef]
encodeConcept "BoardEq SourcePos" = [Content,Dimensions,Array,Board,BoardType,BoardDef,BoardSignature,BoardEquation]
encodeConcept "Feq" = [FunctionDef,FunctionSignature,FunctionEquation]
encodeConcept "Veq" = [AcceptingType,ReturningType,ValueDef,ValueSignature,ValueEquation]
encodeConcept "Ref" = [Reference]
encodeConcept "PosDef" = [Position]
encodeConcept "ForAll" = [PositionAll]
encodeConcept "Index" = [PositionExact] -- should be PosIndex or something like that
encodeConcept "Parameter List" = [Parameter]
encodeConcept "Expr SourcePos" = [Expr]
encodeConcept "I" = [LiteralInt,IntType]
encodeConcept "B" = [LiteralBool,BoolType]
encodeConcept "S" = [LiteralSymbol,SymbolType]
encodeConcept "Xtype" = [ExtendedType]
encodeConcept "Tuple" = [TupleType,Tuple]
encodeConcept "Binop" = [BinOp]
encodeConcept "Plus" = [Add,BinOpArithmetic]
encodeConcept "Minus" = [Sub,BinOpArithmetic]
encodeConcept "Times" = [Mul,BinOpArithmetic]
encodeConcept "Div" = [Div,BinOpArithmetic]
encodeConcept "Less" = [LessThan,BinOpConditional]
encodeConcept "Leq" = [LessThanEqualTo,BinOpConditional]
encodeConcept "Greater" = [GreaterThan,BinOpConditional]
encodeConcept "Geq" = [GreaterThanEqualTo,BinOpConditional]
encodeConcept "Equiv" = [Eq,BinOpConditional]
encodeConcept "NotEquiv" = [NotEq,BinOpConditional]
encodeConcept "Let" = [Let,Name]
encodeConcept "App" = [FunctionApplication,Reference]
encodeConcept "If" = [IfThenElse,Condition,BoolType]
encodeConcept "While" = [While,Condition,BoolType]
encodeConcept "Get" = [Get,Content,Board]
encodeConcept _ = []

step3 :: [String] -> [Concept]
step3 ls = reverse $ _makeUnique (reverse $ concatMap encodeConcept ls)

-- 6. Lastly, using these paths, pick from DB of programs (where we can see the concepts for each snippet), to find one that builds each new concept we want
--      This is an additional dimension, as presenting a given concept can be done in infinitely many programs (OKAY, but that's pretty reasonable to state...)
--      Make sure we have a database of program snippets or sections that are tagged for these concepts, when we add a concept in the path, it should be filled in as a program
db_examples :: [([Concept],String)]
db_examples = map (\(a,b) -> (step3 $ step2 $ step1 a, b)) BDB.db_programs

step5 :: Concept -> [([Concept],String)] -> [(Concept,String)]
step5 _ []      = []
step5 c ((cls,s):ls)  = if c `elem` cls then (c,s) : (step5 c ls) else (step5 c ls)


-- get number of paths from a point
-- then enumerate through ALL of those paths
numPaths :: MC.Dependency Concept -> Int
numPaths (Single c)   = case find (\(a,b) -> a == c) conceptDepGraph of
                        Just(x,y) -> 1 + numPaths y
                        Nothing   -> 0
numPaths (MC.And ls)  = 1 + (sum $ map numPaths ls)
numPaths (MC.Or ls)   = 1 + (length ls) + (sum $ map numPaths ls)


paths :: MC.Dependency Concept -> [[Concept]]
paths (Single c)   = case find (\(a,b) -> a == c) conceptDepGraph of
                        Just(x,y) -> let pty = paths y in
                                     map (\x -> c : x) pty
                        Nothing   -> [[c]]
paths (MC.And ls)  = let singles = filter (\x -> case x of
                                                (Single c) -> True
                                                _          -> False) ls in
                     let singleText = map (\(Single x) -> x) singles in
                     map (\x -> singleText ++ x) $ concatMap (paths) ls
paths (MC.Or ls)   = concatMap paths ls -- this looks fine
