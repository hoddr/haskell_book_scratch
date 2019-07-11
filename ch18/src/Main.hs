module Main where

import Control.Monad (join)
import Control.Applicative ((*>))

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- ex bind in terms of fmap and join
-- join :: Monad m => m (m a) -> m a
-- fmap :: Functor f => (a -> b) -> f a -> f b
bind' :: Monad m => (a -> m b) -> m a -> m b
bind' f x = join $ fmap f x

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn $ "Why hello there: " ++ name

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
    \name -> putStrLn $ "Why hello there: " ++ name

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
     then [x*x, x*x]
     else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
     then join [[x*x], [x*x]]
     else []

-- maybe monad
data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
      then Nothing
      else Just c

mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
       Nothing -> Nothing
       Just nammy ->
         case noNegative age' of
              Nothing -> Nothing
              Just agey ->
                case noNegative weight' of
                     Nothing -> Nothing
                     Just weighty ->
                       weightCheck
                       (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck $ Cow nammy agey weighty

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
    \nammy -> noNegative age' >>=
      \agey -> noNegative weight' >>=
        \weighty -> weightCheck $ Cow nammy agey weighty

-- either monad

