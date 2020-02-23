module Main where

import Data.Monoid

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- library exercises 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = undefined

-- 2
product' :: (Foldable t, Num a) => t a -> a
product' = undefined

-- 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' = undefined

-- 4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = undefined

-- 5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = undefined

-- 6
null' :: (Foldable t) => t a -> Bool
null' = undefined

-- 7
length' :: (Foldable t) => t a -> Int
length' = undefined

-- 8
toList' :: (Foldable t) => t a -> [a]
toList' = undefined

-- 9 use foldMap
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = undefined

-- 10 define foldMap in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' = undefined

-- chapter exercises
-- write foldable for
-- 1
data Constant a b = Constant b

-- 2
data Two a b = Two a b

-- 3
data Three a b c = Three a b c

-- 4
data Three' a b = Three' a b b

-- 5
data Four' a b = Four' a b b b

-- filterF function for foldables
filterF :: (Applicative f
           , Foldable t
           , Monoid (f a))
         => (a -> Bool) -> t a -> f a
filterF = undefined
