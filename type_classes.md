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
- benefit is the `join`, or the `concat` generalization, in `bind`
- `bind` is `join` plus `mapping`

```haskell
class Applicative m => Monad m where
  -- bind
  (>>=) :: m a -> (a -> m b) -> m b
  -- sequencing operator
  (>>) :: m a -> m b -> m b
  -- pure
  return :: a -> m a

join :: Monad m => m (m a) -> m a
-- compare to
concat :: [[a]] -> [a]

-- laws
-- right identity
m >>= return = m

-- left identity
return x >>= f = f x

-- associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

## foldables

```haskell
data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
```

## traversables

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = travaerse id

-- sequenceA
-- traverse

data Either a b = Left a | Right b deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r

instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right y) = f y

  foldr _ z (Left _) = z
  foldr f z (Right y) = f y z

instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y

-- for tuple
instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

-- traversable laws
--1. naturality
-- t . traverse f = traverse (t . f)
--2. identity
-- traverse Identity = Identity
--3. composition
-- traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

-- sequenceA laws
--1. naturality
-- t . sequenceA = sequenceA . fmap t
--2. identity
-- sequenceA . fmap Identity = Identity
--3. composition
-- sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
```

## reader

- way of abstracting out function application and gives method
of doing computation in terms of arg that is not yet supplied
- used most often when a constant value obtained externally
is needed as an argument to a bunch of functions
  - avoids needed to explicitly pass the value around!

```haskell
```
