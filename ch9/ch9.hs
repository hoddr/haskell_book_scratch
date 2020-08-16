module Main where

import Data.Char ( isUpper
                 , toUpper
                 )

main :: IO ()
main = undefined

-- enumFromTo exercise
eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool True False = []
eftBool False True = [False, True]
eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o1 o2 = if o1 <= o2
                  then enumFromTo o1 o2
                  else enumFromTo o2 o1

eftInt :: Int -> Int -> [Int]
eftInt i1 i2 = if i1 <= i2
                  then enumFromTo i1 i2
                  else enumFromTo i2 i1

eftChar :: Char -> Char -> [Char]
eftChar c1 c2 = if c1 <= c2
                   then enumFromTo c1 c2
                   else enumFromTo c2 c1

-- thy fearful symmetry
-- 1 use takeWhile and dropWhile, fn that takes string
-- and returns list of strings, using spaces to sep elems
-- of string into words
myWords :: String -> [String]
myWords = undefined

-- skipped quite a few as they are recap and used to them
-- zip ex
-- original zip

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a,b) : zip as bs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (\a b -> (a, b))

-- chapter exercises
-- 2
filterUppers :: [Char] -> [Char]
filterUppers = filter isUpper

-- 3 capitalize first letter
capFirst :: [Char] -> [Char]
capFirst [] = []
capFirst (x:xs) = (toUpper x) : xs

capRecurse :: [Char] -> [Char]
capRecurse [] = []
capRecurse (x:xs) = (toUpper x) : capRecurse xs

capAndGetFirst :: [Char] -> [Char]
capAndGetFirst [] = []
capAndGetFirst xs = (head $ capFirst xs) : []

capAndGet' :: [Char] -> Char
capAndGet' xs = (head . capFirst) xs

capAndGet'' :: [Char] -> Char
capAndGet'' = head . capFirst

-- write standard function equivalents
-- myAnd
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

-- 1 myOr
myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) = x || myOr xs

-- 2 myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

-- 3 myElem and more
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if e == x
                 then True
                 else myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' e xs = any (\x -> e == x) xs

-- 4 myReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = rev xs []
  where rev [] a = a
        rev (y:ys) a = rev ys (y:a)

-- 5 squish (flattens list of lists into list
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = concat [x, squish xs]

-- 6 squishMap maps function on list and concats result
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = concat . (map f)

-- 7 squishAgain - use squishMap now
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8 myMaximumBy
myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy comp (x:xs) = myInner x xs
  where myInner currMax [] = currMax
        myInner currMax (next:rest) = if (comp currMax next) == GT
                                         then myInner currMax rest
                                         else myInner next rest

-- 9 myMinimumBy
myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy comp (x:xs) = myInner x xs
  where myInner currMin [] = currMin
        myInner currMin (next:rest) = if (comp currMin next) == LT
                                         then myInner currMin rest
                                         else myInner next rest

-- 10 myMaximum and myMinimum
myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined
