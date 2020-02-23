module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

ask :: Reader a a
ask = Reader id
