module Ciphers where

import Data.Char

-- chr
-- ord
-- mod n 26
-- lower starts at 97
-- upper start at 65
caesar :: Int -> String -> String
caesar n = map (encrypt n)
  where encrypt :: Int -> Char -> Char
        encrypt n c
          | isAlpha c && isLower c = chr $ ((mod ((ord c) + n - (ord 'a'))) 26) + (ord 'a')
          | isAlpha c && isUpper c = chr $ ((mod ((ord c) + n - (ord 'A'))) 26) + (ord 'A')
          | otherwise = c

uncaesar :: Int -> String -> String
uncaesar n = map (decrypt n)
  where decrypt :: Int -> Char -> Char
        decrypt n c
          | isAlpha c && isLower c = chr $ ((mod ((ord c) - n - (ord 'a'))) 26) + (ord 'a')
          | isAlpha c && isUpper c = chr $ ((mod ((ord c) - n - (ord 'A'))) 26) + (ord 'A')
          | otherwise = c

type Key = String

-- meet at dawn
-- ally al lyal
-- mppr ae oywy
vigenere :: Key -> String -> String
vigenere = undefined

unvigenere :: Key -> String -> String
unvigenere = undefined

mapKeyToString :: Key -> String -> (Char, Char)
mapKeyToString = undefined
