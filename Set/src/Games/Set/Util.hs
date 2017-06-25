{-# LANGUAGE Haskell2010 #-}

module Games.Set.Util (
    uniq,
    choose,
    allValues,
    twoValues,
    (-->)
) where

import Data.List (group, sort)


allValues, twoValues :: (Enum a, Bounded a) => [a]

allValues = [ minBound .. maxBound ]

twoValues = take 2 allValues


-- sorts and eliminates duplicates, thereby giving a unique representation
uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = map head . group . sort


-- n choose k
choose = chooseByTriangle


n `chooseByTriangle` k = triangle !! n !! k

triangle = iterate nextRow [1]
  where
    nextRow givenRow = 1 : zipWith (+) givenRow (tail givenRow) ++ [1]


n `chooseByFaculty` k = faculty n `quot` (faculty k * faculty (n - k))

faculty = product . enumFromTo 1


-- tupel sugar
infixr 0 -->

a --> b = (a, b)


