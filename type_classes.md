# Short notes on type classes of interest

## monoid

- binary, associative, identity
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
