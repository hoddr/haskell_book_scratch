module CH7 where

main :: IO ()
main = undefined

-- let's write code

-- same type
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = fst $ divMod x 10
        d = snd $ divMod xLast 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where xLast = x `div` 100
        d = xLast `mod` 10

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
                      True -> y
                      False -> x

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b = y
  | otherwise = x

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 5
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

-- 6
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show
