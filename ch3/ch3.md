# chapter 3 work

## scope exc

1. yes
2. not unless defined earlier
3. no; where is d defined for r
4. yes

## syntax errors

```haskell
--- 1 no (infix)
[1,2,3] ++ [4,5,6]

--- 2 no (' ' for chars, not strings)
"<3" ++ " Haskell"

--- 3 yes
```

## ch8 ex

### reading syntax

```haskell
--- 1 a yes, b no
(++) [1,2,3] [4,5,6]
--- c yes, d no (missing "), e yes, f yes
--- g no move 4 outside ", h yes

--- 2 a -> d, b -> c, c -> e, d -> a, e -> b
```

#### building functions

```haskell
--- 1 a
addExl s = s ++ "!"
--- b
"String" !! 4
--- c
drop 9 "string"

--- 2
addExcl s = s ++ "!"
takeFifthLetter s = s !! 4
lastWord

--- 3
thirdLetter :: String -> Char
thirdLetter s = s !! 2

--- 4
takeIndex :: Int -> Char
takeIndex n = "Curry is awesome!" !! n

--- 5
badSlicing = concat [take 7 $ drop 9 s, drop 5 $ take 9 s, take 5 s]
  where s = "Curry is awesome!"

--- 6
module Reverse where

rvrs :: String -> String
rvrs s = concat [take 7 $ drop 9 s, drop 5 $ take 9 s, take 5 s]

main :: IO ()
main = print $ rvrs "Curry is awesome!"
```
