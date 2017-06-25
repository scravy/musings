{-# LANGUAGE Haskell2010 #-}

module Games.Set.Card where

import Games.Set.Common

data Card = Card {
  color :: Color,
  count :: Count,
  shape :: Shape,
  fill  :: Fill
} deriving (Eq, Ord)

instance Show Card where
    show (Card c n s f) = concatMap show [ fromEnum c, fromEnum n, fromEnum s, fromEnum f ]

instance Read Card where
    readsPrec _ xs
        | length xs < 4 = []
        | any (\x -> x < 0 || x > 2) digits = []
        | otherwise = [ (card, drop 4 xs) ]
      where
        card = Card (toEnum c) (toEnum n) (toEnum s) (toEnum f)
        digits = map (read . (: "")) $ take 4 xs
        [ c, n, s, f ] = digits

instance Enum Card where
    toEnum = decodeCard
    fromEnum = encodeCard

instance Bounded Card where
    maxBound = toEnum 80
    minBound = toEnum 0

encodeCard :: Card -> Int
encodeCard card = sum $ zipWith (*) values powers
  where
    values = [
        fromEnum $ color card,
        fromEnum $ count card,
        fromEnum $ shape card,
        fromEnum $  fill card
      ]
    powers = map (base ^) [ 0 .. ]

decodeCard :: Int -> Card
decodeCard num = card
  where
    card = Card {
      color = toEnum d1,
      count = toEnum d2,
      shape = toEnum d3,
      fill = toEnum d4
    }
    [ d1, d2, d3, d4 ] = zipWith decode (replicate numProperties num) powers
    decode value power = value `rem` (power * base) `quot` power
    powers = map (base ^) [ 0 .. ]


