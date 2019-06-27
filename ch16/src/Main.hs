module Main where

import Test.QuickCheck

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- ex be kind
-- 1. *
-- 2. * -> *, * -> * -> *
-- 3. * -> * -> *

data FixMePls a =
              FixMe
              | Pls a
              deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- 16.6 the good, the bad, and the ugly

data WhoCares a = ItDoesnt
                | Matter a
                | WhatThisIsCalled
                deriving (Eq, Show)

-- law-abiding
-- instance Functor WhoCares where
--   fmap _ ItDoesnt = ItDoesnt
--   fmap _ WhatThisIsCalled = WhatThisIsCalled
--   fmap f (Matter a) = Matter (f a)

-- law-breaking
-- instance Functor WhoCares where
--   fmap _ ItDoesnt = WhatThisIsCalled
--   fmap f WhatThisIsCalled = ItDoesnt
--   fmap f (Matter a) = Matter (f a)
--
-- composition (bad)
-- data CountingBad a = Heisenberg Int a
--                    deriving (Eq, Show)
--
-- instance Functor CountingBad where
--   fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)
-- composition (good)
data CountingGood a = Heisenberg Int a
                    deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

-- ex heavy lifting
-- 1.
a = fmap (+1) $ read "[1]" :: [Int]
-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- 3.
c = fmap (*2) (\x -> x - 2)
-- 4.
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
-- 5.
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap ("123" ++) (fmap show ioi)
    in fmap (*3) changed

data Or a b = First a
            | Second b
            deriving (Eq, Show)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- quick checks
functorIdentity :: (Functor f, Eq (f a))
                => f a
                -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f)
               => (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- ex instances of functor
-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

identityId :: Identity Int -> Bool
identityId = functorIdentity
testIdentityId = quickCheck identityId

identityComp x = functorCompose (+1) (*2) (x :: Identity Int)
testIdentityComp = quickCheck identityComp

-- 2.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

pairId :: Pair Int -> Bool
pairId = functorIdentity
testPairId = quickCheck pairId

pairComp x = functorCompose (+1) (*2) (x :: Pair Int)
testPairComp = quickCheck pairComp

-- 3.
data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

twoId :: Two Int Int -> Bool
twoId x = functorIdentity x
testTwoId = quickCheck twoId

twoComp x = functorCompose (+1) (*2) (x :: Two Int Int)
testTwoComp = quickCheck twoComp

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

threeId :: Three Int Int Int -> Bool
threeId = functorIdentity
testThreeId = quickCheck threeId

threeComp x = functorCompose (+1) (*2) (x :: Three Int Int Int)
testThreeComp = quickCheck threeComp

-- 5.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

threeId' :: Three' Int Int -> Bool
threeId' = functorIdentity
testThreeId' = quickCheck threeId'

threeComp' x = functorCompose (+1) (*2) (x :: Three' Int Int)
testThreeComp' = quickCheck threeComp'

-- 6.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a
        , Arbitrary b
        , Arbitrary c
        , Arbitrary d
        ) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

fourId :: Four Int Int Int Int -> Bool
fourId = functorIdentity
testFourId = quickCheck fourId

fourComp x = functorCompose (+1) (*2) (x :: Four Int Int Int Int)
testFourComp = quickCheck fourComp

-- 7.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z b) = Four' x y z (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

fourId' :: Four' Int Int -> Bool
fourId' = functorIdentity
testFourId' = quickCheck fourId'

fourComp' x = functorCompose (+1) (*2) (x :: Four' Int Int)
testFourComp' = quickCheck fourComp'

-- 8. No. Has wrong kind of * and not * -> *
