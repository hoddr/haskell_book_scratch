module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Arbitrary (shrink2, arbitrary2)

import Control.Monad (join, (>=>))
import Control.Applicative ((*>))

import System.Random (randomRIO)

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

-- either monad ex
data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First f <*> _ = First f
  Second f <*> x = fmap f x

instance Monad (Sum a) where
  return = pure
  First f >>= _ = First f
  Second f >>= x = x f

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

-- ch18 exercises
-- write monad instances and check
-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = do
    return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

tNope :: Nope (Int, Int, Int)
tNope = NopeDotJpg

testNope = do
  quickBatch (monad tNope)
  quickBatch (functor tNope)
  quickBatch (applicative tNope)

-- 2.
data BahEither b a = PLeft b
                   | PRight a
                   deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PLeft a) = PLeft a
  fmap f (PRight b) = PRight (f b)

instance Applicative (BahEither b) where
  pure = PRight
  PLeft a <*> _ = PLeft a
  PRight a <*> x = fmap a x

instance Monad (BahEither b) where
  return = pure
  PLeft a >>= _ = PLeft a
  PRight a >>= x = x a

-- instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
--   arbitrary = do
--     r <- getRandom
--     if r == 0
--        then PRight <$> arbitrary
--        else PLeft <$> arbitrary
--
-- getRandom :: IO ()
-- getRandom = do
--   r <- randomRIO (0, 1)
--   r

-- instance (Eq a, Eq b) => EqProp (BahEither b a) where
--   (=-=) = eq
--
-- tEither :: Either (Int, Int, Int) (Int, Int, Int)
-- tEither = undefined
--
-- testEither = do
--   quickBatch (monad tEither)
--   quickBatch (functor tEither)
--   quickBatch (applicative tEither)

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) a = fmap f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

testId = do
  let trigger :: Identity (Int, String, Int)
      trigger = undefined
  verboseBatch (monad trigger)
  verboseBatch (functor trigger)
  verboseBatch (applicative trigger)

-- 4.
data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> x = Nil
  _ <*> Nil = Nil
  Cons f fs <*> as = undefined
  -- wish you could use = [ f x | f <- fs, x <- as ]
  -- (Cons f fs) <*> as = Cons (fmap f as) (fs <*> as)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a as) >>= f = join $ Cons (f a) (f <$> as)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return a

testList = do
  let trigger :: List (Int, Int, Int)
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (monad trigger)
  quickBatch (applicative trigger)

-- write functions
-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

-- 5. close but no cigar... found same type sig in Control.Monad with forM
-- meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh [] _ = return []
-- meh (a:as) f = join $ (return a >>= f) (meh as f)

-- 6. relies on above
