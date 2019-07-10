# Short notes on type classes of interest

## monoid

- binary, associative, identity
- can have multiple monoids for types (e.g. sum and product for numbers)
- must be Semigroup (`(<>)`) defined
- define `mempty` and `mappend`
- laws:
- laws that apply to Semigroup `(<>)` also apply to `mappend`

```haskell
-- left id
mappend mempty x = x

-- right id
mappend x mempty = x

-- associativity
mappend x (mappend y z) = mappend (mappend (x y) z

mconcat = foldr mappend mempty
```

## semigroup

- set + binary + associative
- generalizes monoids - may not have idenity
- define `(<>)`, which is equivalent to `mappend` (usually)
- only the associativity law now

```haskell
instance Semigroup Foo where
  (<>) = undefined

(a <> b) <> c = a <> (b <> c)
```

## functors

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$>)

-- identity
fmap id == id

-- composition
fmap (f . g) == fmap f . fmap g

functorIdentity :: (Functor f, Eq (f a)) =>
                    f a
                    -> Bool
functorIdentity f =
  fmap id f == f
functorCompose :: (Eq (f c), Functor f) =>
                      (a -> b)
                  -> (b -> c)
                  -> f a
                  -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)
```

- maps over or through an object (box) without altering the container
- aka list will retain same length
- abstracts the act of mapping over container contents
- laws
  - identity
  - composition

## applicatives

- monoidal functors
-

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  -- also known as apply, or ap
  (<*>) :: f (a -> b) -> f a -> f b

-- Control.Applicative
-- liftA, liftA2, liftA3

-- laws
-- identity
pure id <*> v = v

-- composition
-- apply then compose versus compose then apply
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- homomorphism
-- structure-preserving map b/tw 2 algeabraic structures
-- applying fn in some struct to value in some struct is same as applying
-- a fn to value w/out affecting outside struct
pure f <*> pure x = pure (f x)

-- interchange
-- u is fn in some struct
u <*> pure y = pure ($ y) <*> u
```

## monads

- applicative functors plus extra stuff!

```haskell
class Applicative m => Monad m where
  -- bind
  (>>=) :: m a -> (a -> m b) -> m b
  -- sequencing operator
  (>>) :: m a -> m b -> m b
  -- pure
  return :: a -> m a
```
