{-# LANGUAGE Haskell2010 #-}

module Games.Set.Triplet (
    Triplet,
    triplet,
    fromTriplet
) where

import Games.Set.Common
import Games.Set.Card

import Data.List (sort)


newtype Triplet = Triplet Int deriving (Eq, Ord)

instance Show Triplet where
    show = show . fromTriplet

triplet c1 c2 c3 = Triplet $ encodeTriplet (c1', c2', c3')
  where
    [ c1', c2', c3' ] = sort [ c1, c2, c3 ]

fromTriplet (Triplet num) = decodeTriplet num

toTriplet (c1, c2, c3) = triplet c1 c2 c3

toInt (Triplet num) = num


encodeTriplet (c1, c2, c3) = sum $ zipWith (*) values powers
  where
    values = map encodeCard [ c1, c2, c3 ]
    powers = map (tbase ^) [ 0 .. ]
    tbase = base ^ numProperties

decodeTriplet num = (decodeCard c1, decodeCard c2, decodeCard c3)
  where
    [ c1, c2, c3 ] = zipWith decode (replicate 3 num) powers
    decode value power = value `rem` (power * tbase) `quot` power
    powers = map (tbase ^) [ 0 .. ]
    tbase = base ^ numProperties


