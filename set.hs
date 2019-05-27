import Prelude
import Data.List

data Digit = Zero | One | Two
  deriving (Eq, Ord, Enum, Bounded)

data Card = Card Digit Digit Digit Digit
  deriving (Eq, Ord)

isDigit :: Char -> Bool
isDigit = (`elem` "012")

toList :: Card -> [Digit]
toList (Card a b c d) = [a, b, c, d]

isSet :: [Card] -> Bool
isSet cs = case cs of
  [_, _, _] -> all check (zip3 a b c)
   where
    [a, b, c] = map toList cs
    check (d1, d2, d3)
      | d1 == d2 && d2 == d3 = True
      | triNot d1 d2 d3 = True
      | otherwise = False
  _ -> False

triNot :: Eq a => a -> a -> a -> Bool
triNot a b c = a /= b && b /= c && a /= c

instance Show Card where
  show = map (head . show . fromEnum) . toList

instance Read Card where
  readsPrec _ xs = case xs of
    (a : b : c : d : rs) | all isDigit ds -> [(card, rs)]
     where
      card = Card (rd a) (rd b) (rd c) (rd d)
      rd = toEnum . read . (:[])
      ds = [a, b, c, d]
    _ -> []

-- From a sorted list, take the remaining ones that are greater than given specimen
rm :: Ord a => a -> [a] -> [a]
rm x = dropWhile (<= x)

main = do
  dat <- filter (not . null) . map (takeWhile isDigit) . lines <$> readFile "set.txt"
  let ns = sort (map read dat :: [Card])
      ss = [ (n1, n2, n3) | n1 <- ns, n2 <- rm n1 ns, n3 <- rm n2 ns, isSet [n1, n2, n3] ]
  mapM_ print ns
  putStrLn "---"
  mapM_ print ss

