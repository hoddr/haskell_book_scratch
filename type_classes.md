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
