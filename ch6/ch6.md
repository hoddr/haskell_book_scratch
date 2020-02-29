# ch6

## day of week

```haskell
data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
      weekday == weekday'
      && dayOfMonth == dayOfMonth'
```

## equal instances

- see ch6.hs file

## will they work

1. yes, 5
2. yes, LT
3. No, two different types
4. yes, False

## ch ex

### multiple choice

1. c
2. c
3. a
4. c
5. a

### does it type check

- code fixes in ch6.hs

1. no
2. no, no Eq derived
3. Blah, Woot; Error (no type comparison to ints); No order
4. no, missing Object for constructor

### given data type, what can we do

1. no, Papu takes Rocks, not bare String
2. yes
3. yes
4. no; no Ord class derived

### match the types

- kind of an odd task; not sure what is desired here

### type-kwon-do two

```haskell
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith f i a = f a + fromInteger i
```
