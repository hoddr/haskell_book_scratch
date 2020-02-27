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

```haskell
```
