module P2021.General where

import Debug.Trace

-- program in concrete syntax w/ a name
type ConcreteProgram = (String,String)

-- subsumable relation
class Subsumable a where
  -- if the 1st subsumes the 2nd
  subsumes :: a -> a -> Bool

  -- attempts to reduce a list by applying subsumption
  subsume :: [a] -> [a]
  subsume ls = foldl (\x y -> subsume' x y) ls ls
               where
                 subsume' :: Subsumable a => [a] -> a -> [a]
                 subsume' [] _ = []
                 subsume' (y:ys) x  | x `subsumes` y = subsume' ys x
                                    | otherwise      = y : subsume' ys x

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

-- Hand written join
join :: String -> [String] -> String
join _ [] = ""
join c (x:ls) = if length ls > 0 then x ++ c ++ (join c ls) else x
