module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- can't figure out how to make it work, as
-- (read $ show x) :: Int
-- doesn't work
-- 402
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = undefined
