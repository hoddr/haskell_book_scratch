# ch7 ex

## grab bag

1. all the same
2. d
3.
  a. `f = \n -> n + 1`
  b. `addFive = \x -> \y -> ((if x > y then y else x) + 5)`
  c. `mflip f x y = f y x`

## variety pack

1. `k :: (a, b) -> a`
2. `k2 :: Num a => ([Char], a) -> [Char]` no
3. k3

```haskell
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
```

## case practice

```haskell
functionC x y = case x of
  x > y -> x
  _ -> y

ifEvenAdd2 n = case n of
  even n -> n + 2
  _ -> n

nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0
```

## artful dodgy

```haskell
dodgy :: (Num a) => a -> a -> a
OneIsOne :: Num a => a -> a
oneIsTwo :: Num a => a -> a
```
Types are correct; skipping plugging values into ghci

## guard duty

1. all will match `otherwise` branch
2. no, order matters
3. b
4. [a]
5. `pal :: [a] -> Bool` or `pal :: Foldable t => t a -> Bool`
6. c
7. `Num a`
8. `numbers :: Num a => a -> a

## ch ex

### multiple choice

1. d
2. `Char -> String -> [String]`
3. `(Ord a, Num a) => a -> Bool`
4. b
5. a

### let's write code

see ch7.hs
```haskell
```
