module Main where

import Control.Monad
import Data.Monoid
import qualified Data.Semigroup as S
import Test.QuickCheck

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mri :: Bull -> Bool)


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

monoidAssoc :: (Eq m, Monoid m) =>
               m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance S.Semigroup Bull where
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

instance S.Semigroup (First' a) where
  (<>) = undefined

instance Monoid (First' a) where
  mempty = undefined
  mappend = undefined

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend
