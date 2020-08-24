module Main where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\next acc ->
                      case next of
                           (DbDate utc) -> utc : acc
                           _ -> acc) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\next acc -> case next of
                       (DbNumber i) -> i : acc
                       _ -> acc) []


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb dbs = sum xs / (fromIntegral $ length xs)
  where xs = map fromIntegral $ filterDbNumber dbs


-- ch 10 exercises
stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

stopVowelStop :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStop ss vws = [(x, y, z) | x <- ss, y <- vws, z <- ss]

onlyPs :: [(Char, Char, Char)] -> [(Char, Char, Char)]
onlyPs = filter (\(x, y, z)-> x == 'p')

-- 2
-- divides number of chars by number of words
seekritFunc :: [Char] -> Int
seekritFunc x = div (sum (map length (words x)))
                    (length (words x))

betterFunc :: [Char] -> Double
betterFunc x = (/) (sum (map (fromIntegral . length) (words x)))
                   ((fromIntegral . length) (words x))

-- rewriting as folds
-- point-free if possible
-- myOr
myOr :: [Bool] -> Bool
myOr = foldr (||) False

--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = foldr (\n a -> a || (pred n)) False

-- via fold
myElem :: Eq a => a -> [a] -> Bool
myElem test = foldr (\n acc -> acc || (test == n)) False

-- via any
-- any :: Foldable t => (a -> Bool) -> t a -> Bool
myElem' :: Eq a => a -> [a] -> Bool
myElem' test = any (\n -> test == n)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\n acc -> f n : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\n acc -> if pred n
                      then n : acc
                      else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\n acc -> f n ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- these are weird...
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\acc n -> case f n acc of
                      GT -> acc
                      _ -> n) x xs

-- these are weird...
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\acc n -> case f n acc of
                                             LT -> acc
                                             _ -> n) x xs

main :: IO ()
main = undefined
