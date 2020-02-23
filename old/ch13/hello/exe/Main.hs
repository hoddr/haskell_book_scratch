module Main where

import DogsRule ( dogs )
import Hello ( sayHello )

import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please input your name: "
  name <- getLine
  sayHello name
  dogs
