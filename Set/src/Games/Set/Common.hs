{-# LANGUAGE Haskell2010 #-}

module Games.Set.Common where


data Color = Red | Green | Purple deriving (Eq, Show, Enum, Bounded, Ord)
data Count = One | Two | Three deriving (Eq, Show, Enum, Bounded, Ord)
data Shape = Diamond | Squircle | Tilde deriving (Eq, Show, Enum, Bounded, Ord)
data Fill  = Empty | Hatched | Filled deriving (Eq, Show, Enum, Bounded, Ord)


numProperties = 4 :: Int

base = 3 :: Int


