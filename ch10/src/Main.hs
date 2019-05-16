module Main where

import Data.Time

main :: IO ()
main = undefined

-- exercises: understanding folds
-- 1. foldr (*) 1 [1..5] => b and c
-- 2. foldl (flip (*)) 1 [1..3]
-- (((1 * 1) * 2) * 3) == 6
-- 3. c
-- 4. a
-- 5.
-- a. foldr (++) "" ["woot", "WOOT", "woot"], add "" as initial value
-- b. foldr max [] "fear is the little death" -> foldr max [] $ words "fear is the little death"
-- c. foldr and True [False, True]  -> foldr (&&) True [False, True] OR
-- and $ True : [False, True]
-- d. foldr (||) True [False, True]
-- should probably be foldr (||) False [False, True]
-- e. foldl ((++) .show) "" [1..5] -> foldl (flip $ (++) . show) "" [1..5]
-- f. foldr const 'a' [1..5] -> foldr const 'a' ['1'..'5']
-- g. foldr const 0 "tacos" -> const 0 "tacos"
-- h. foldl (flip const) 0 "burritos" -> foldl (flip const) '0' "burritos"
-- i. foldl (flip const) 'z' [1..5] -> ['1'..'5']
-- foldl has performance impacts due to unconditional evalution of spine
-- should use foldl' for long lists/infinite
--
-- database processing
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
  , DbNumber 123
  , DbNumber 1234
  , DbString "Hello, world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDates []
  where filterDates :: DatabaseItem -> [UTCTime] -> [UTCTime]
        filterDates (DbDate t) xs = t : xs
        filterDates _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNums []
  where filterNums (DbNumber n) xs = n : xs
        filterNums _ xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = sum / num
  where sum = fromIntegral $ sumDb xs
        num = fromIntegral $ length xs

-- scan exercises
-- 1. use `take 20 $`
-- 2. use `takeWhile (\n -> n < 100)`
-- 3. infinite!
factorial :: [Integer]
factorial = scanl (*) 1 [1..]

-- ch 10 exercises
