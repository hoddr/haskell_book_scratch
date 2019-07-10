module Main where

import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes

main :: IO ()
main = putStrLn "Hello, Haskell!"

f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")
               ]

g y = lookup y [ (7, "sup?")
               , (8, "chirs")
               , (9, "aloha")
               ]

h z =
  lookup z [(2, 3), (5, 6), (7, 8)]

m x =
  lookup x [(4, 10), (8, 13), (1, 9001)]

-- ex lookups
-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4. not exactly correct, bizarre example...
xs = [1, 2, 3]

ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> x' <*> y'')

-- applicative identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- instance (Semigroup a) => Semigroup (Identity a) where
--   (<>) (Identity x) (Identity y) = Identity (x <> y)
--
-- instance (Monoid a) => Monoid (Identity a) where
--   mempty = Identity (mempty)
--   mappend (Identity x) (Identity y) = Identity (mappend x y)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

-- applicative constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = (Constant x)

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (x <> y)

-- maybe applicative
validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen
                             then Nothing
                             else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = case mkName n of
                    Nothing -> Nothing
                    Just n' ->
                      case mkAddress a of
                           Nothing -> Nothing
                           Just a' ->
                             Just $ Person n' a'

-- instance Functor Maybe where
-- fmap _ Nothing = Nothing
-- fmap f (Just a) = Just (f a)
-- instance Applicative Maybe where
-- pure = Just
-- Nothing <*> _ = Nothing
-- _ <*> Nothing = Nothing
-- Just f <*> Just a = Just (f a)

-- fixer upper ex
-- 1.
x1 = const <$> Just "Hello" <*> pure "World"
-- 2.
x2 = (,,,)
  <$> Just 90
  <*> Just 10
  <*> Just "Tierness"
  <*> (pure [1,2,3])

-- list applicative - brain block at the moment

-- data List a = Nil
--             | Cons a (List a)
--             deriving (Eq, Show)
--
-- instance Functor List where
--   fmap _ Nil = Nil
--   fmap f (Cons x xs)= (Cons (f x) $ fmap f xs)
--
-- instance Applicative List where
--   pure x = Cons x Nil
--   (<*>) _ Nil = Nil
-- --   (<*>) Nil xs = xs
--   (<*>) (Cons f fs) (Cons x xs) = Cons (fmap f x) $ fs <*> xs
--
--append :: List a -> List a -> List a
--append Nil ys = ys
--append (Cons x xs) ys =
--Cons x $ xs `append` ys
--
--fold :: (a -> b -> b) -> b -> List a -> b
--fold _ b Nil
-- = b
--fold f b (Cons h t) = f h (fold f b t)
--concat' :: List (List a) -> List a
--concat' = fold append Nil
---- write this one in terms
---- of concat' and fmap
-- flatMap :: (a -> List b)
-- -> List a
-- -> List b
--flatMap f as = undefined
--Use the above and try us

-- validation
data Validation e a = Failure' e
                    | Success' a
                    deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' f) (Failure' x) = Failure' (f <> x)
  (<*>) (Success' f) (Failure' x) = Failure' x
  (<*>) (Failure' f) (Success' x) = Failure' f
  (<*>) (Success' f) (Success' x) = Success' (f x)

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

t :: Validation String Int
t = Success' 5

-- skipped zipList

-- chapter exercises
-- 1. []
-- pure :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]
-- 2. IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b
-- 3. (,) a
-- pure :: a -> (a,)
-- (<*>) :: ((a -> b), ) -> (a, ) -> (b, )
-- 4. (->) e
-- pure :: a -> (->) a
-- (<*>) :: (->) (a -> b) -> (->) a -> (->) b
--
-- instances
-- 1. Pair a
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2. Two a b
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two f g) (Two a b) = Two (f <> a) (g b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- done for now
