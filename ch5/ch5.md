# ch5 ex

## type matching

```haskell
not :: Bool -> Bool
length :: [a] -> Int
concat :: [[a]] -> [a]
head :: [a] -> a
(<) :: Ord a => a -> a -> Bool
```

## type arguments

```haskell
--- 1 a
--- 2 d
--- 3 d
--- 4 c
--- 5 a
--- 6 e
--- 7 d
--- 8 a
--- 9 c
```

## parametricity

```haskell
--- 1 (cannot!)
--- 2
test :: a -> a -> a
test x y = x
test x y = y

--- 3
--- like snd
test2 :: a -> b -> b
test2 x y = y
```

## apply yourself

```haskell
--- 1
--- has to go to
myConcat :: [Char] -> [Char]

--- 2
myMult :: Fractional a => a -> a

--- 3
myTake :: Int -> [Char] -> [Char]

--- 4
myCom :: Int -> Bool

--- 5
myAlph :: Char -> Bool
myAlph x = x < 'z'
```

## chapter ex

### multiple choice

1. c
2. a
3. b
4. c

### determine the type

```haskell
--- 1a
(*9) 6 :: Num a => a

--- b
:: Num a => (a, [Char])

--- c
:: (Integer, [Char])

--- d
:: Bool (False)

--- e
5 :: Int

--- f
False :: Bool

--- 2
w :: Num a => a

--- 3
z :: Num a => (a -> a) -> a

--- 4
f :: Fractional a => a

--- 5
f :: [Char]
```

### does it compile

```haskell
--- 1
bigNum x = (^) 5 x
wahoo = bigNum 10

--- 2
--- fine as is

--- 3
a = (+)
b = 5
c = a b 10
d = a c 200

--- 4
--- where are b and c declared?
```

### type var or specific type constructor? (pg 153)

1. constrained, full, concrete, concrete
2. full, concrete, concrete
3. full, constrained, concrete
4. full, full, concrete

### write a type sig

```haskell
--- 1 (like head)
functionH :: [a] -> a
functionH :: Foldable t => t a -> a

--- 2
functionC :: (Ord a) => a -> a -> Bool

--- 3
functionS :: (a, b) -> b
```

### given type, write fn

```haskell
--- 1
i :: a -> a
i = id

--- 2
c :: a -> b -> a
c x _ = x

--- 3
c'' :: b -> a -> b
c'' = c

--- 4
r :: [a] -> [a]
r = tail
r = reverse

--- 5
co :: (b -> c) -> (a -> b) -> a -> c
co g f x = g $ f a

--- 6
a :: (a -> c) -> a -> a
a _ x = x

--- 7
a' :: (a -> b) -> a -> b
--- (->)
a' f x = f x
```

### fix it

```haskell
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y)
         then fstString x
         else sndString y
       where x = "Singin"
             y = "Somewhere"

--- 3
module Arith3Broken where

main :: IO()
main = do
  print $ 1 + 2
  putStrLn 10
  print $ negate (-1)
  print $ (+) 0 (negate 1)
```

### type-kwon-do

```haskell
--- 1
h :: Int -> Char
h = g . f

--- 2
e = w . q

--- 3
xform (a, b) = (xz a, yz b)

--- 4
munge f g x = fst $ g $ f x
```
