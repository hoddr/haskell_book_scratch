module Cipher (
  caesar,
  unCaesar
) where

-- ord chr functions
import Data.Char ( chr
                 , isUpper
                 , ord
                 )

-- caesar to encrypt
caesar :: Int -> String -> String
caesar = map . shifter

-- a starts at 97
-- A starts at 65
shifter :: Int -> Char -> Char
shifter shiftBy c = if c == ' '
                       then ' '
                       else if isUpper c
                         then shift 65
                         else shift 97
  where shift n = chr $ n + mod (ord c + (abs shiftBy) - n) 26

-- unCaesar to decrypt
unCaesar :: Int -> String -> String
unCaesar = map . unshifter

unshifter :: Int -> Char -> Char
unshifter shiftBy c = if c == ' '
                         then ' '
                         else if isUpper c
                           then unshift 65
                           else unshift 97
  where unshift n = chr $ n + mod (ord c - (abs shiftBy) - n) 26
