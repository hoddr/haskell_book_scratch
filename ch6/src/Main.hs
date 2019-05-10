module Main where

main :: IO ()
main = undefined

data TisAnInteger =
  TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers =
  Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two x y) (Two a b) = x == a && y == b

data StringOrInt =
  TisAnInt Int
  | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

data Pair a = Pair a a
instance Eq t => Eq (Pair t) where
  (==) (Pair x y) (Pair a b) = a == x && b == y

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple w x) (Tuple y z) = w == y && x == z

data Which a =
             ThisOne a
             | ThatOne a
instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

data EitherOr a b =
                  Hello a
                  | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _ = False

-- multiple choice 6.14
-- 1. c
-- 2. Ord is a subclass of Eq, b
-- 3. Ord a => a -> a -> Bool, a
-- 4. c or b (Integeral a => (a, a))
-- 5. a
--
-- type checks
-- 1. No, fixed below
-- data Person = Person Bool deriving Show
-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show person)
--
-- 2. No, fixed below (needed Eq)
-- data Mood = Blah | Woot deriving (Eq, Show)
-- settleDown x = if x == Woot
--                   then Blah
--                   else x
--
-- 3. a, anything that is instance of Eq or Show
-- 3. b, Error, Num Mood doesn't exist
-- 3. c, Error, not instance of Ord
--
-- 4. yes
-- type Subject = String
-- type Verb = String
-- type Object = String
--
-- data Sentence =
--   Sentence Subject Verb Object
--   deriving (Eq, Show)
--
-- s1 = Sentence "dogs" "drool"
-- s2 = Sentence "Julie" "loves" "dogs"
--
-- dataype declaration exc
data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)
-- 1. phew = Papu "choses" True, no not using type constructors
-- 2. truth = Papu (Rocks "chomskydoz") (Yeah True), yes
-- 3. equalityForAll :: Papu -> Papu -> Bool
-- equalityForAll p p' = p == p', yes, all things deriving Eq
-- 4. no, not instances of Ord
--
-- match the types
-- 1. NO
-- i :: Num a => a
-- i = 1
-- j :: a
-- j = 1
-- 2. NO
-- f :: Float
-- f = 1.0
-- g :: Num a => a
-- g = 1.0
-- 3. Yes
-- f :: Float
-- f = 1.0
-- g :: Fractional a => a
-- g = 1.0
-- 4. Yes, as instance RealFrac Float where ...
-- f :: Float
-- f = 1.0
-- g :: RealFrac a => a
-- g = 1.0
-- 5. Yes, but second has tigher constraint
-- freud :: a -> a
-- freud x = x
-- gfreud :: Ord a => a -> a
-- gfreud x = x
-- 6. Yes, but second can only handle Int types!
-- 7. No
-- 8. Yes, specialized to Int
-- 9. Yes, specialized to Int since Int is instance of Ord
-- 10. Not same, as first is more specific. It would still compile
-- 11. Yes, but more general until applied as `mySort` specializes to Char type
--
-- type-kwon-do 2
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b
-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f const a = (f a) + (fromIntegral const)
