module CH8 where

main :: IO ()
main = undefined

mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 (n + 11)
