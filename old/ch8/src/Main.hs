module Main where

main :: IO ()
main = undefined

-- integral division via subtraction
-- multiplication via addition
-- 3 * 4 = 3 + (3 * 3) = 3 + 3 + (3 * 2) = 3 + 3 + 3 + (3 * 1) = 3 + 3 + 3 + 3 = 12
-- 12 / 4 = (12 - 4) / 4 = (12 - 4 - 4) / 4 = 4 / 4 = (12 - 4 - 4 - 4) / 4 = 0
-- actually care about the number of times the above happens!
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy = div

dividedBy' :: Integral a => a -> a -> (a, a)
dividedBy' num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- chapter exercises
-- review of types
-- 1. d
-- 2. b
-- 3. d
-- 4. b
-- review currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
-- 1. "woops mrow wooho!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow blue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue"
-- 6. "are mrow Pugs mrow awesome"
-- recursion
-- 1. (skipped)
-- 2. recursive sum from 1 to n, n being arg
recurseSum :: (Eq a, Num a) => a -> a
recurseSum 0 = 0
recurseSum 1 = 1
recurseSum n = n + recurseSum (n - 1)

-- 3. multiplies two integral nums using recursive summation
recurseMult :: (Integral a) => a -> a -> a
recurseMult x 0 = 0
recurseMult x 1 = x
recurseMult x y = x + recurseMult x (y - 1)

-- fixing dividedBy
data DividedResult =
                   Result Integer
                   | DividedByZero

instance Show DividedResult where
  show DividedByZero = "DividedByZero"
  show (Result x) = "Result " ++ show x

dividedByFixed :: Numerator -> Denominator -> DividedResult
dividedByFixed x 0 = DividedByZero
dividedByFixed x y
  | x > 0 && y < 0 = negateResult $ dividedByFixed x (negate y)
  | x < 0 && y > 0 = negateResult $ dividedByFixed (negate x) y
  | x < 0 && y < 0 = dividedByFixed (negate x) (negate y)
  | otherwise = go x y 0
  where go x y count
          | x < y = Result count
          | otherwise = go (x - y) y (count + 1)

negateResult :: DividedResult -> DividedResult
negateResult DividedByZero = DividedByZero
negateResult (Result x) = Result (negate x)

-- McCarthy 91
-- MC(n) = { n - 10 if n > 100 MC(MC(n + 11)) if N <= 100 }
mc91 :: (Integral a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 (n + 11)

-- nums into words see src/word_number.hs

