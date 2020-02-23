module WarmUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupledApplicative :: [Char] -> ([Char], [Char])
tupledApplicative = (,) <$> rev <*> cap

-- tupledMonadic :: [Char] -> ([Char], [Char])
-- tupledMonadic xs = do
--   x <- cap xs
--   y <- rev xs
--   return (x, y)
