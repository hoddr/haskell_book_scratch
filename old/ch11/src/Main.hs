module Main where

import Data.List (intercalate, sort)
import Data.Char (toUpper)
import Data.List.Split (splitOn)

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

-- exercise: programmers
data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)

data Programmer = Programmer
                { os :: OperatingSystem
                , lang :: ProgLang }
                deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer x y | x <- allOperatingSystems, y <- allLanguages ]

-- exponentiation in order
-- data Quantum = Yes | No | Both deriving (Eq, Show)

-- convert :: Quantum -> Bool
-- convert = undefined

-- 1.
-- convert Yes = True
-- convert No = True
-- convert Both = True

-- 2.
-- convert Yes = True
-- convert No = True
-- convert Both = False

-- 3.
-- convert Yes = True
-- convert No = False
-- convert Both = True

-- 4.
-- convert Yes = True
-- convert No = False
-- convert Both = False

-- 5.
-- convert Yes = False
-- convert No = True
-- convert Both = True

-- 6.
-- convert Yes = False
-- convert No = True
-- convert Both = False

-- 7.
-- convert Yes = False
-- convert No = False
-- convert Both = True

-- 8.
-- convert Yes = False
-- convert No = False
-- convert Both = False

-- exercises the quad
-- 1. 4 + 4 = 8
-- 2. 4 * 4 = 16
-- 3. 4 ^ 4 = 256
-- 4. 2 * 2 * 2 = 2 ^ 3 = 8
-- 5. a -> b -> c = (2 ^ 2) ^ 2 = 8
-- 6. (4 ^ 4) ^ 2 = 256 ^ 2 = 65536
--
-- binary tree section
data BinaryTree a =
                  Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

-- write map for binary tree
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- binary tree to list
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: Ord a => BinaryTree a -> [a]
inorder tree = sort $ preorder tree

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf)
                2
                (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
                  then putStrLn "Preorder fine!"
                  else putStrLn "Preorder fails."

testInorder :: IO ()
testInorder = if inorder testTree == [1,2,3]
                 then putStrLn "Inorder fine!"
                 else putStrLn "Preorder fails."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1,3,2]
                   then putStrLn "Postorder fine!"
                   else putStrLn "Postorder fails."

runTests :: IO ()
runTests = do
  testPreorder
  testInorder
  testPostorder

-- foldr binary tree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc tree = foldr f acc $ preorder tree

-- chapter exercises
-- 1. a
-- 2. c
-- 3. b
-- 4. c

-- vigenere cipher (see ciphers.hs)
--
-- as-patterns
-- 1.
isSubSeqOf :: (Eq a) => [a] -> [a] -> Bool
isSubSeqOf (s:[]) t = elem s t
isSubSeqOf (s:ss) t = if elem s t
                         then isSubSeqOf ss $ dropWhile (\c -> c /= s) t
                         else False

-- 2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capTuple . words
  where capTuple :: String -> (String, String)
        capTuple s@(x:xs) = (s, (toUpper x) : xs)

-- language ex
-- 1.
capitalizeWord :: String -> String
capitalizeWord (s:ss) = toUpper s : ss

capitalizeSentence s = inner $ words s
  where inner (w:ws) = capitalizeWord w : ws

-- 2.
-- capitalizeParagraph :: String -> String
-- capitalizeParagraph s = intercalate "." $ map capitalizeSentence $ splitOn "." s
--
-- phone game (skipped)
-- razor (skipped)
