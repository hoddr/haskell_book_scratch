module Main where

import Control.Monad
import Data.Monoid (Monoid)
import Data.Semigroup
import Test.QuickCheck (quickCheck, arbitrary, Arbitrary, frequency)

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
  mconcat [
          e
          , "! he said "
          , adv
          ," as he jumped into his car "
          , noun
          , " and drove off with his "
          , adj
          , " wife."
          ]

monoidAssoc :: (Eq m, Monoid m, Semigroup m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m)
                    => m
                    -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend = (<>)

type BullMappend = Bull -> Bull -> Bull -> Bool

-- ex optional monoid

data Optional a =
                  Nada
                  | Only a
                  deriving (Eq, Show)

instance Monoid a
         => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only y) = Only y
  mappend (Only x) Nada = Only x
  mappend (Only x) (Only y) = Only $ mappend x y

-- ex  maybe another monoid

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) y = y
  (<>) x  _ = x


instance Monoid (First' a) where
  mempty = (First' Nada)
  mappend = (<>)

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (2, return (First' (Only a)))
              , (1, return (First' Nada))
              ]

-- semigroup exercises 15.15
-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

testTrivial :: IO ()
testTrivial = quickCheck (semigroupAssoc :: TrivAssoc)

-- 2.
newtype Identity a = Identity a

instance (Show a) => Show (Identity a) where
  show (Identity a) = "(Identity " <> show a <> ")"

instance (Eq a) => Eq (Identity a) where
  (==) (Identity x) (Identity y) = x == y

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity (mappend x y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String
                   -> Identity String
                   -> Identity String
                   -> Bool

testIdentity :: IO ()
testIdentity = quickCheck (semigroupAssoc :: IdentityAssoc)

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two x2 y2) = Two (x <> x2) (y <> y2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two w x) (Two y z) = Two (mappend w y) (mappend x z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- instance (Show a, Show b) => Show (Two a b) where
--   show = "(Two " <> show a <> " " <> show b <> ")"
--
-- instance (Eq a, Eq b) => Eq (Two a b) where
--   (==) (Two x y) (Two x2 y2) = x == x2 && y == y2

type TwoAssoc = Two String String
             -> Two String String
             -> Two String String
             -> Bool

testTwo :: IO ()
testTwo = quickCheck (semigroupAssoc :: TwoAssoc)

-- 4. too similar to above
-- 5. too similar to above

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup (BoolConj) where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _ = BoolConj False

instance Monoid (BoolConj) where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary (BoolConj) where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool

testBoolConj = quickCheck (semigroupAssoc :: BoolConjAssoc)

-- 7.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup (BoolDisj) where
  (<>) (BoolDisj True) (BoolDisj True) = BoolDisj True
  (<>) (BoolDisj True) _ = BoolDisj True
  (<>) _ (BoolDisj True) = BoolDisj True
  (<>) _ _ = BoolDisj False

instance Monoid (BoolDisj) where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary (BoolDisj) where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

type BoolDisjAssoc = BoolDisj
                  -> BoolDisj
                  -> BoolDisj
                  -> Bool

testBoolDisj = quickCheck (semigroupAssoc :: BoolDisjAssoc)

-- 8.
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Fst a) (Snd b) = Snd b
  (<>) (Fst a) (Fst b) = Fst a
  (<>) (Snd a) _ = Snd a

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
--   arbitrary = do
--     a <- arbitrary
--     b <- arbitrary
--     frequency [ (1, return Fst a)
--               , (1, return Snd b)
--               ]

-- 9.
-- newtype Combine a b = Combine { unCombine :: (a -> b) }
--
-- instance Semigroup (Combine a b) where
--   (<>) (Combine f) (Combine g) = Combine (f . g)

-- 11.
-- data Validation a b =
--                     Failure a | Success b
--                     deriving (Eq, Show)
--
-- instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
--   (<>) (Success x) (Success y) = Success (x <> y)
--   (<>) (Success x) y = y
--   (<>) x (Success y) = x
--   (<>) (Failure x) (Failure y) = Failure (x <> y)
--
-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
--   arbitrary = undefined
--
-- type ValidationAssoc = Validation String Int
--                      -> Validation String Int
--                      -> Validation String Int
--                      -> Bool

-- testValidation = quickCheck (semigroupAssoc :: validationAssoc)

-- monoid exercises (see above if they are combined)
-- 1. Trivial
testTrivialMonoid = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)

-- 2. Identity a
-- 3, 4, 5 above
-- 8
newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem { runMem = f }) (Mem { runMem = g }) = Mem $
    \s -> (((fst (f s)) <> (fst (g s))), (snd $ g (snd (f s))))

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

testMemMonoid = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
