module General where

-- | Program in concrete syntax w/ a name
type ConcreteProgram = (String,String)

-- | Typeclass for objects that have a subsumption relation
class Subsumable a where
  -- if the 1st subsumes the 2nd
  subsumes :: a -> a -> Bool
  subsumes _ _ = False

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

-- | Removes duplicates in a list, but does not preserve order
makeUnique :: (Eq a) => [a] -> [a]
makeUnique = reverse . uniqueInSameOrder . reverse

-- | Joins list of strings using a provided separator
join :: String -> [String] -> String
join _ [] = ""
join c (x:ls) = if length ls > 0 then x ++ c ++ (join c ls) else x
