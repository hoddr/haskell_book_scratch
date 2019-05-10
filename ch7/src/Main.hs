module Main where

main :: IO ()
main = undefined

-- exercises: variety pack
k (x, y) = x
k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)
-- a. (t1, t2) -> (t1)
-- b. k1 :: Integer
-- b. k2 :: [Char]
-- c. k1 and k3
-- 2.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- case exercises
-- 1. functionC x y = if (x > y) then x else y
functionC x y = case x > y of
                     True -> x
                     False -> y
-- 2. ifEvenAdd2 n = if even n then (n + 2) else n
ifEvenAdd2 n = case even n of
                    True -> n + 2
                    False -> n
-- 3. nums x = case compare x 0 of LT -> -1 GT -> 1
nums x = case compare x 0 of
              EQ -> 0
              LT -> -1
              GT -> 1

-- higher order functions
data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
       GT -> reportBoss e e'
       EQ -> putStrLn "Neither employee is the boss."
       LT -> (flip reportBoss) e e'

-- dodgy
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-- guard duty
-- 1. all will get caught by first otherwise guard
-- 2. order matters. say swap 'A' and 'B', an 'A' would get thrown as 'B'
-- remainder are all straight forward
--
-- function composition
-- chapter exercises
-- multiple choice
-- 1. d
-- 2. d
-- 3. b
-- 4. b
-- 5. a
-- let's write code
-- 1a.
-- original
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigitDivMod :: Integral a => a -> a
tensDigitDivMod x = snd (divMod (fst $ x `divMod` 10) 10)
-- 1b. yes
-- 1c.
hunsD :: Integral a => a -> a
hunsD x = mod (div x 100) 10
-- 2. case expression
foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
                      True -> y
                      False -> x

-- 2. guard
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b == True = y
  | otherwise = x

-- 2. given
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, b) = (f a, b)

-- 4. see arith4.hs

