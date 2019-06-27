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

