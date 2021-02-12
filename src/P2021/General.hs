module P2021.General where

import Debug.Trace

-- program in concrete syntax w/ a name
type ConcreteProgram = (String,String)

-- | Removes duplicates from a list, whilst preserving order
uniqueInSameOrder :: (Eq a) => [a] -> [a]
uniqueInSameOrder [] = []
uniqueInSameOrder (x:ls) | elem x ls = uniqueInSameOrder ls     -- drop, already present
                         | otherwise = x : uniqueInSameOrder ls -- keep it

-- | Removes duplicates from a list, whilst preserving order (reporting dups)
uniqueInSameOrder' :: (Eq a, Show a) => [a] -> [a]
uniqueInSameOrder' [] = []
uniqueInSameOrder' (x:ls) | elem x ls = trace ("\n\nProgram " ++ (show x) ++ ", intent was duplicated, but kept\n\n") $ x : uniqueInSameOrder' ls     -- drop, already present
                          | otherwise = x : uniqueInSameOrder' ls -- keep it


-- | Removes duplicates from a list, whilst preserving order (reporting dups by NAME)
uniqueInSameOrder'' :: (Eq a, Show a) => [(a,[a])] -> [(a,[a])]
uniqueInSameOrder'' [] = []
uniqueInSameOrder'' (x@(a,b):ls) | elem b (map snd ls) = trace ("\n\nFiltered out program " ++ (show a) ++ ", intent was duplicated in programs " ++ (show $ map fst $ filter (\(_,b') -> b' == b) ls) ++ "\n\n") $ uniqueInSameOrder'' ls
                          | otherwise = x : uniqueInSameOrder'' ls -- keep it

-- | Removes duplicates in a list, but does not preserve order
makeUnique :: (Eq a) => [a] -> [a]
makeUnique = reverse . uniqueInSameOrder . reverse
