module Main where

main :: IO ()
main = undefined

-- exercises: dog types
-- 1. type constructor
-- 2. Doggies :: * -> *
-- 3. Doggies String :: *
-- 4. Husky 10 :: Num a => Doggies a
-- 5. Husky (10 :: Integer) :: Doggies Integer
-- 6. Mastiff "Scooby Doo" :: Doggies String
-- 7. yes (that name is used for both...)
-- 8. DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9. DogueDeBordeaux String

-- exercises: vehicles
data Price = Price Integer deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
yourCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 10)

-- 1. myCar :: Vehicle
-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined

-- 4. won't work, hence left as undefined
-- 5. above contains modifications

-- exercise: cardinality
-- 1. data PugType = PugData -> 1
-- 2. 3
-- 3. 65536 (2^16)
-- 4. Integer must be specialized to get cardinality - theoretically it is infinite), Int is rather large (2^64)
-- 5. 8 refers to number of bits available to represent the numbers in that set, so 2^8 represents all possible combinations of 8 0's or 1's (e.g. 00001100)

-- exercises: for example
-- 1. Example, data constructor not in scope: Example
-- 2. Shows data declaration, instance for Show (safe), so yes
-- 3. data MyExample2 = Example Int deriving Show
-- 4. Requesting that Int is provided

-- exercises: logic goats
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = tooMany n

-- 1.
newtype Foo = Foo (Int, String) deriving (Eq, Show)

instance TooMany Foo where
  tooMany (Foo (x, y)) = tooMany x

-- 2.
newtype Combo = Combo (Int, Int) deriving (Eq, Show)

instance TooMany Combo where
  tooMany (Combo (x, y)) = tooMany (x + y)

-- 3.
newtype (Num a, TooMany a) => Special a = Special (a, a) deriving (Eq, Show)

instance (Num a, TooMany a) => TooMany (Special a) where
  tooMany (Special (x, y)) = tooMany (x * y)

-- exercises: pity the bool
-- 1. Either (Big or Small) where Big has 2 possibilities and each has 2, so 4
-- 2. Can be any Int8 value (256 opts) OR one of True or False, so 256 + 2 = 258

-- data Person = MkPerson String Int deriving (Eq, Show)
-- jm = MkPerson "julie" 108
-- ca = MkPerson "chris" 16
--
-- namae :: Person -> String
-- namae (MkPerson s _) = s
--
-- data PersonRec =
--   Person { name :: String
--          , age :: Int }
--          deriving (Eq, Show)
-- exercise: garden growing

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
            deriving Show

-- normal form (sum of products)
-- data Garden =
--   Gardenia Gardener
--   | Daisy Gardener
--   | Rose Gardener
--   | Lilac Gardener
--   deriving Show
