module Main where

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
