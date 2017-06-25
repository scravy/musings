{-# LANGUAGE Haskell2010 #-}

module Games.Set.Common where

import Games.Set.Card
import Games.Set.Triplet

import Data.List

-- checks whether a given triplet is a set
isSet t = satisfies t color && satisfies t count && satisfies t shape && satisfies t fill 
  where
    satisfies triplet p = checksOut (p c1) (p c2) (p c3)
      where
        (c1, c2, c3) = fromTriplet triplet
    checksOut p1 p2 p3 = allEqual || allDifferent
      where
        allEqual = p1 == p2 && p2 == p3
        allDifferent = p1 /= p2 && p2 /= p3 && p3 /= p1

-- all 3 ^ 4 = 81 cards
allCards = allValues :: [Card]

-- all 81 choose 3 = 85320 triplets
allTriplets = uniq [ triplet c1 c2 c3
                   | c1 <- allCards, c2 <- allCards, c3 <- allCards,
                     c1 /= c2, c2 /= c3, c1 /= c3 ]

-- all (81 choose 2) / 3 = 1080 sets
-- (choosing 2 out of 81 chooses all sets thrice)
allSets = filter isSet allTriplets

-- a presumably maximal set
maximal = [ Card { color = c, count = n, shape = s, fill = f }
          | c <- twoValues, n <- twoValues, s <- twoValues, f <- twoValues ]

-- all cards that would complete a set of cards such that it had at least 1 set
completions :: [Card] -> [Card]
completions cards = uniq [ missing c1 c2 | c1 <- cards, c2 <- cards, c1 /= c2 ]

-- finds all the sets in a given set of cards
sets :: [Card] -> [Triplet]
sets cards = uniq [ triplet c1 c2 (missing c1 c2)
                  | c1 <- cards, c2 <- cards, c1 /= c2, missing c1 c2 `elem` cards ]

-- determine the third card from two given cards that together form a set
missing :: Card -> Card -> Card
missing c1 c2 = Card { color = c, count = n, shape = s, fill = f }
  where
    c = third (color c1) (color c2)
    n = third (count c1) (count c2)
    s = third (shape c1) (shape c2)
    f = third (fill c1) (fill c2)
    third x y
      | x == y = x
      | otherwise = head $ filter (\a -> a /= x && a /= y) allValues

numProperties = 4
base = 3

sanityChecks = [

    let lengths = [
            length (allValues :: [Color]),
            length (allValues :: [Count]),
            length (allValues :: [Shape]),
            length (allValues :: [Fill])
          ]
    in
    "number of values are equal to base" --> all (== base) lengths,

    let allCardsEncoded = map encodeCard allCards
        allCardsDecoded = map decodeCard allCardsEncoded
    in
    "encoding/decoding cards" --> allCardsDecoded == allCards,

    let pairs = [ (n, k) | n <- [ 0 .. 10 ], k <- [ 0 .. n ] ]
        byTriangle = map (uncurry chooseByTriangle) pairs
        byFaculty = map (uncurry chooseByFaculty) pairs
    in
    "n choose k by triangle vs by faculty" --> byTriangle == byFaculty,

    let maximalAndCompletions = uniq (maximal ++ completions maximal)
        allUnique = uniq allCards
    in
    "the maximal set really is maximal" --> maximalAndCompletions == allUnique,

    "none of the cards completing maximal is in maximal" --> 
        (not . any id) [ c == m | c <- completions maximal, m <- maximal ],

    "allTriplets should already be unique" -->
        length allTriplets == length (uniq allTriplets),

    "allSets is already unique" -->
        length allSets == length (uniq allSets),

    "allTriplets should have 81 choose 3 entries" -->
        length allTriplets == 81 `choose` 3,

    "allSets should have 81 choose 2 / 3 entries" -->
        length allSets == 81 `choose` 2 `quot` 3,

    let encodedTuples = map (encodeTriplet . fromTriplet) allTriplets
    in
    "encoding/decoding of triplets should work" -->
        map (toTriplet . decodeTriplet) encodedTuples == allTriplets
  ]


renderCard :: Card -> String
renderCard (Card color count shape fill) = concat [
    "<div class=\"Card ",
    show color,
    " ",
    show count,
    " ",
    show shape,
    " ",
    show fill,
    "\">",
    "<span class=\"Symbol First\"></span>",
    "<span class=\"Symbol Second\"></span>",
    "<span class=\"Symbol Third\"></span>",
    "</div>"
  ]

renderTriplet :: Triplet -> String
renderTriplet triplet = concat [
    "<div class=\"Triplet\">",
    renderCard c1,
    renderCard c2,
    renderCard c3,
    "</div>"
  ] where (c1, c2, c3) = fromTriplet triplet

main :: IO ()
main = do
    putStrLn $ concat [
        "<html>",
        "<head>",
        "<link rel=\"stylesheet\" href=\"SET.css\">",
        "</head>",
        "<body>"
      ]
    mapM_ (putStrLn . renderCard) allCards



