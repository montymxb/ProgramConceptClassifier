{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
--
-- Modern revision of the tool
--
module P2021.R32 where

import Parser.Parser
import Data.Data
import Data.Either
import P2021.General
import qualified Data.Set as S
import Data.List

--
-- BoGL Stuff
--
import P2021.Bogl_Specifics

import Language.Syntax (Game)
import Text.Parsec.Pos
import qualified Data.Set as DS
import qualified DB.BoglDB as BDB

-- terms are names in this context, both terminal & nonterminal
type Term = String

-- Object with attributes
type Object = String

-- attribute of an object
type Attribute = String

-- Pair of sets, (A subset G, B subset M)
-- A is the extent: set of all objs that have all the attributes in B
  -- For all 'a' in G. 'a' in A iff (For all 'b' in B. b(a))
-- B is the intent: set of all attributes that match to all objects in A
  -- For all 'b' in M. 'b' in B iff (For all 'a' in A. b(a))
data FormalConcept = FormalConcept ([Object],[Attribute])
  deriving Eq

instance Ord FormalConcept where
  compare a b = if a == b then EQ else if compareFormalConceptsByIntents a b then LT else GT

-- formal context (G,M,I)
-- G = Set of all objects (programs)
-- M = Set of all attributes (terms)
-- I = Subset of (G X M), Relation that links elements of G to elements of M
type FormalContext = ([Object],[Attribute],[(Object,Attribute)])

-- Ordering for Formal Concepts
data OrderBy =
  OrderByIntents |
  OrderByExtents

data ConceptsBy =
  ConceptsByIntents |
  ConceptsByExtents

type ParsingFunction a = ([ConcreteProgram] -> [(String, a)])

type KnownPrograms = [ConcreteProgram]
type GoalPrograms  = [ConcreteProgram]
type CoursePrograms= [ConcreteProgram]

-- Configuration for performing Formal Concept Analysis
data FCA a = FCA OrderBy ConceptsBy (ParsingFunction a) KnownPrograms GoalPrograms CoursePrograms

-- ([ConcreteProgram] -> [(String, a)]) -> [ConcreteProgram] -> [ConcreteProgram]
r32 :: (Data a, Show a) => FCA a -> IO ()
r32 (FCA ob cb programParser kps gps cps) = do
  -- parse the programs that are understood
  let parsedKPS = programParser kps
  -- parse the goal programs that are desired to be understood
  let parsedGPS = programParser gps
  -- parse course programs that are available as extents
  let parsedCPS = programParser cps

  -- extract terms for the known list of programs, preserving names
  let knownIntents = map astToTerms parsedKPS
  -- extract terms for the goal list of programs, preserving names
  let goalIntents = map astToTerms parsedGPS
  -- extract terms for the course list of programs, preserving names
  let courseIntents = map astToTerms parsedCPS

  -- produce total context
  let totalContext = mkFormalContext (uniqueInSameOrder (knownIntents ++ goalIntents ++ courseIntents))

  -- TODO tmp
  --let totalContext = filterGeneralAttributes totalContext_

  -- produce Known + Goal context
  --let kgContext = mkFormalContext (uniqueInSameOrder (knownIntents ++ goalIntents))

  -- produce total concepts from total context
  let totalConcepts = getConceptsFromContext cb totalContext

  -- produce Known + Goal concepts from K+G context
  --let kgConcepts = uniqueInSameOrder $ mkFormalConceptFromIntents kgContext
  -- find the smallest & largest concept in K+G list
  --let smallest = minimum kgConcepts
  --let largest  = maximum kgConcepts
  -- remove all concepts beyond this range of concepts
  --let filteredConcepts = if length kgConcepts > 1 then filter (\x -> x >= smallest && x <= largest) totalConcepts else filter (\x -> x <= largest) totalConcepts

  -- produce a formal context from everything together
  --let formalContext = mkFormalContext (uniqueInSameOrder (knownIntents ++ goalIntents))
  -- find formal concepts by objs
  --let formalConcepts = uniqueInSameOrder $ mkFormalConceptFromIntents formalContext
  --putStrLn $ (show parsedP1s) ++ (show parsedP2s)
  --putStrLn "\n\n"
  --putStrLn $ (show terms1) ++ (show terms2)
  --putStrLn $ show formalConcepts
  putStrLn $ "Num Course Progs: " ++ (show $ length cps)
  putStrLn $ "Num Course Objects: " ++ (show $ length ((\(x,_,_) -> x) totalContext))
  putStrLn $ "Num Course attributes: " ++ (show $ length ((\(_,x,_) -> x) totalContext))
  --putStrLn $ "Num K+G Progs: " ++ (show $ length (kps ++ gps))
  --putStrLn $ "Num K+G Objects: " ++ (show $ length ((\(x,_,_) -> x) kgContext))
  --putStrLn $ "Num K+G attributes: " ++ (show $ length ((\(_,x,_) -> x) kgContext))
  putStrLn $ "Num Domain Concepts: " ++ (show $ length totalConcepts)
  --putStrLn $ "Num K+G Concepts: " ++ (show $ length kgConcepts)
  --putStrLn $ "Num Concepts in K+G Range: " ++ (show $ length filteredConcepts)
  --putStrLn $ show formalContext
  --putStrLn $ show $ map (\(FormalConcept (n,_)) -> n) (sort formalConcepts)
  putStrLn $ showConcepts (sort totalConcepts)


showConcepts :: [FormalConcept] -> String
showConcepts [] = "\n"
showConcepts ((FormalConcept (n,a)):ls) = "Extent: " ++ (show n) ++ "\nIntent: " ++ (show a) ++ "\n\n" ++ (showConcepts ls)


getConceptsFromContext :: ConceptsBy -> FormalContext -> [FormalConcept]
getConceptsFromContext ConceptsByIntents fc = uniqueInSameOrder $ mkFormalConceptFromIntents fc
getConceptsFromContext ConceptsByExtents fc = uniqueInSameOrder $ mkFormalConceptFromExtants fc


-- Convert program in AST to list Terms
astToTerms :: (Data a) => (Object,a) -> (Object,[Term])
astToTerms (name,p) = (name, uniqueInSameOrder $ mkTermsFromData p)

-- Traverse immediate subterms recursively to discover all terms used
mkTermsFromData :: Data a => a -> [Term]
mkTermsFromData val =
                let tn = show $ typeOf val in -- 1. get type name
                -- 2. get constructor name
                let cn = if isAlgType (dataTypeOf val) then show $ toConstr val else tn in
                -- 3. same for subterms
                let immediateAnds = gmapQ (\d -> show $ typeOf d) val in
                -- run this for sub-terms as well
                let subs = concat $ gmapQ (\d -> mkTermsFromData d) val in
                tn : cn : subs ++ immediateAnds

-- 3) Function that produces a formal context from program names & terms
mkFormalContext :: [(Object,[Attribute])] -> FormalContext
mkFormalContext ls = let g = map (\(obj,_)  -> obj) ls in
                     let m = uniqueInSameOrder $ concatMap (\(_,atrs) -> atrs) ls in
                     let i = uniqueInSameOrder $ concatMap (\(obj,atrs) -> map (\atr -> (obj,atr)) atrs) ls in
                     (g, m, i)

-- remove general attributes that are shared by all programs
filterGeneralAttributes :: FormalContext -> FormalContext
filterGeneralAttributes (g,m,i) = let filt = filter (\(_,b) -> (length (filter (\(_,b') -> b' == b) i)) < (length g)-1) i in
                                  if length g > 1 then (g,m,filt) else (g,m,i)

-- ' : A -> B, returns set of attributes which apply to all objects in A
a' :: [Object] -> FormalContext -> [Attribute]
a' [] _ = []
a' (x:ls) fc@(_,_,i) = let filt = filter (\(o,_) -> o == x) i in
                       let cm = map snd filt in
                       let rset = S.fromList (a' ls fc) in
                       let atrs = if null rset then cm else S.toList $ (S.fromList cm) `S.intersection` rset in
                       uniqueInSameOrder $ atrs

-- ' : B -> A, returns set of objects which have all attributes in B
b' :: [Attribute] -> FormalContext -> [Object]
b' [] _ = []
b' (x:ls) fc@(_,_,i) = let filt = filter (\(_,a) -> a == x) i in
                       let cm = map fst filt in
                       let rset = S.fromList (b' ls fc) in
                       let atrs = if null rset then cm else S.toList $ (S.fromList cm) `S.intersection` rset in
                       uniqueInSameOrder $ atrs

-- find formal concepts through object analysis
-- Set A of objs can determine a concept
-- (A'',A')
-- A'' is the extent closure
mkFormalConceptFromExtants :: FormalContext -> [FormalConcept]
mkFormalConceptFromExtants fc@(g,m,i) = map (\obj -> FormalConcept (b' (a' [obj] fc) fc, a' [obj] fc)) g

-- find formal concepts through attribute analysis
-- or set B of attributes determines a concept
-- (B',B'')
-- B'' is the intent closure
mkFormalConceptFromIntents :: FormalContext -> [FormalConcept]
mkFormalConceptFromIntents fc@(g,m,i) = map (\atr -> FormalConcept (b' [atr] fc, a' (b' [atr] fc) fc)) m

-- partially ordered by extants
-- (A1,B1) <= (A2,B2) iff A1 is a subset A2
compareFormalConceptsByExtents :: FormalConcept -> FormalConcept -> Bool
compareFormalConceptsByExtents (FormalConcept (a,_)) (FormalConcept (b,_)) = (S.fromList a) `S.isSubsetOf` (S.fromList b)

-- partially ordered by intents (these 2 should be equivalent ways of doing this)
-- (A1,B1) <= (A2,B2) iff B2 is a subset B1
compareFormalConceptsByIntents :: FormalConcept -> FormalConcept -> Bool
compareFormalConceptsByIntents (FormalConcept (_,a)) (FormalConcept (_,b)) = (S.fromList b) `S.isSubsetOf` (S.fromList a)

--

-- 5) Function that builds a ConceptLattice from [FormalConcept] using ConceptGraph
-- [FormalConcept] -> ConceptGraph b FormalConcept
  -- there is an algo in the book somewhere on FCA
  -- display this once done, giving some arbitrary name to each or display in some other way


-- 6) Add a function that identifies the outer & inner fringe based on [Known FormalConcept] and the CG
-- ConceptGraph ... -> [Known FormalConcept]
  -- from here, report the outer & inner fringe, and leave it to the user to proceed separately
  -- these would be the items directly preceding the highest level of the known area (or just the boundary in general)
  -- and same for the ones just before
  -- this can be identified if we have a sub-lattice that works.

-- 7) Record as (Concept Graph of FCs,[Known FCs])
  -- for practical observation need a way to observe the possible set of paths that will be generated (all possible progressions)
  -- just recursively build this up, should be okay?

-- At this point, make sure you have some decent examples, and demonstrate the system, the theory, as well as the reasoning behind it
-- 5-10 minute talk, and a brief overview of how this would work in a browser based system
