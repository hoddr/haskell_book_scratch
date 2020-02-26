# ch4 ex

## mood swing ex

```haskell
data Mood = Blah | Woot deriving Show

--- 1 Mood
--- 2 Blah, Woot
--- 3, 4 fixed below
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
```

## find the mistakes

```haskell
not True && True
not (x == 6)
(1 * 2) > 5 - fine
["Merry"] > ["Happy"]
"1,2,3" ++ "look at me!"
```

## ch4 ex

```haskell
--- 1
length :: [a] -> Int
--- actual is length :: Foldable t => t a -> Int

--- 2
--- a) 5 b) 3 c) 2 d) 5

--- 3
--- 6 / 3 works; 6 / length [1,2,3] doesn't as length -> Int

--- 4
6 `div` length [1,2,3]

--- 5 Bool, True

--- 6 a) Num, 5; b) Bool, False

--- 7 a) yes, True, b) no, list of different types
--- 7 c) yes, 5 d) yes, False e) no, 9 is not Bool

--- 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

--- 9
myAbs :: Integer -> Integer
myAbs n = if n >= 0
            then n
            else (-1 * n)

--- 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
```

### correcting syntax

```haskell
--- 1
x = (+)
f xs = x w 1
  where w = length xs

--- 2
\x -> x

--- 3
f :: (a, a) -> a
f x = fst x
```

### match function names to types

1. c
2. b
3. a
4. d
