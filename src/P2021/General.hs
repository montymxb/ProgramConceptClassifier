module P2021.General where

-- program in concrete syntax w/ a name
type ConcreteProgram = (String,String)

-- | Removes duplicates from a list, whilst preserving order
uniqueInSameOrder :: (Eq a) => [a] -> [a]
uniqueInSameOrder [] = []
uniqueInSameOrder (x:ls) | elem x ls = uniqueInSameOrder ls -- drop, already present
                   | otherwise = x : uniqueInSameOrder ls-- keep it

-- | Removes duplicates in a list, but does not preserve order
makeUnique :: (Eq a) => [a] -> [a]
makeUnique = reverse . uniqueInSameOrder . reverse
