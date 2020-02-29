# type classes and reqs for definitions

Able to derive some, including:

- Eq
- Ord
- Enum
- Bounded
- Read
- Show

## Eq

```haskell
--- minimal complete definition: either (==) or (/=)
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True
```

## Num

```haskell
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

Defaults:
- Num Integer
- Real Integer
- Enum Integer
- Integral Integer
- Fractional Double
- RealFrac Double
- Floating Double
- RealFloat Double

### Integer

```haskell
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
```

### Fractional

```haskell
class (Num a) => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
```

## Ord

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
```

## Enum

```haskell
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
```

## Show

- minimal requires `show` or `showPrec`

```haskell
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
```

## Read

- more later - possibly dangerous!

```haskell
```
