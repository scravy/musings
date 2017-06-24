{-# LANGUAGE Haskell2010 #-}

import Data.List

data Color = Red | Green | Purple deriving (Eq, Show, Enum, Bounded, Ord)
data Count = One | Two | Three deriving (Eq, Show, Enum, Bounded, Ord)
data Shape = Diamond | Squircle | Tilde deriving (Eq, Show, Enum, Bounded, Ord)
data Fill  = Empty | Hatched | Filled deriving (Eq, Show, Enum, Bounded, Ord)

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

newtype Triplet = Triplet Int deriving (Eq, Ord)

instance Show Triplet where
    show = show . fromTriplet

triplet c1 c2 c3 = Triplet $ encodeTriplet (c1', c2', c3')
  where
    [ c1', c2', c3' ] = sort [ c1, c2, c3 ]

fromTriplet (Triplet num) = decodeTriplet num

toTriplet (c1, c2, c3) = triplet c1 c2 c3


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

allValues, twoValues :: (Enum a, Bounded a) => [a]
allValues = [ minBound .. maxBound ]
twoValues = take 2 allValues

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

-- sorts and eliminates duplicates, thereby giving a unique representation
uniq :: (Eq a, Ord a) => [a] -> [a]
uniq = map head . group . sort

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

encodeCard card = sum $ zipWith (*) values powers
  where
    values = [
        fromEnum $ color card,
        fromEnum $ count card,
        fromEnum $ shape card,
        fromEnum $  fill card
      ]
    powers = map (base ^) [ 0 .. ]

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


choose = chooseByTriangle


n `chooseByTriangle` k = triangle !! n !! k

triangle = iterate nextRow [1]
  where
    nextRow givenRow = 1 : zipWith (+) givenRow (tail givenRow) ++ [1]


n `chooseByFaculty` k = faculty n `quot` (faculty k * faculty (n - k))

faculty = product . enumFromTo 1


infixr 0 -->

a --> b = (a, b)

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


