module Main where

import Data.Bool (bool)
import Data.Char

main :: IO ()
main = undefined

--
-- class Enum a where
--   succ :: a -> a
--   pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int
--   enumFrom :: a -> [a]
--   enumFromThen :: a -> a -> [a]
--   enumFromTo :: a -> a -> [a]
--   enumFromThenTo :: a -> a -> a -> [a]
-- exc enumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = case compare x y of
                  GT -> []
                  LT -> myRange x y
                  EQ -> [x]

eftInt :: Int -> Int -> [Int]
eftInt x y = case compare x y of
                  GT -> []
                  LT -> myRange x y
                  EQ -> [x]

eftChar :: Char -> Char -> [Char]
eftChar x y = case compare x y of
                   GT -> []
                   LT -> myRange x y
                   EQ -> [x]

myRange :: (Enum a, Eq a) => a -> a -> [a]
myRange x y
  | x == y = [x]
  | otherwise = x : myRange (succ x) y

-- thy fearful symmetry
-- 1.
myWords :: String -> [String]
myWords "" = []
myWords s = (takeWhile (\c -> c /= ' ') s) : myWords next
  where toAdd = takeWhile (\c -> c /= ' ') s
        next = drop 1 $ dropWhile (\c -> c /= ' ') s

-- 2.
myLines :: String -> [String]
myLines = lines

-- 3. splitting at certain char c = '' or char c = '\n', so splitAt recursive
-- square cube
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1.
tuplesAbove = [(x,y) | x <- mySqr, y <- myCube]
tuplesAbove' = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
lengthAbove = length tuplesAbove'

-- more bottoms (parital, as most are simple eval jobs)
-- 7.
negateIf3 :: (Num a, Eq a) => [a] -> [a]
negateIf3 = map (\x -> bool x (-x) (x == 3))

-- filtering
-- 1.
mutliples3 = filter (\x -> (rem x 3) == 0) [1..30]
-- 2.
legnthMultiples3 = length . filter (\x -> (rem x 3) == 0)
-- 3.
myFilter :: String -> [String]
myFilter = filter (\x -> and [x /= "the", x /= "a", x /= "an"]) . words

-- zipping exercises
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (\x y -> (x, y))

-- chapter exercises
-- 2.
filterUppers :: String -> String
filterUppers = filter isUpper
