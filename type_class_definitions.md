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
