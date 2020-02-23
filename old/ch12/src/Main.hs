module Main where

import Data.List.Split (splitOn)
import Data.List (intercalate)

main :: IO ()
main = undefined

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
                   True -> Right age
                   False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
                     True -> Right name
                     False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

-- 12.5 chapter ex
-- 1. id :: a -> a
-- *
-- 2. * and * -> *
--
-- string processing

replaceThe :: String -> String
replaceThe = intercalate " " . map unpack . map notThe . splitOn " "
  where unpack Nothing = "a"
        unpack (Just a) = a

notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

-- 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = counter . words
  where counter [] = 0
        counter (x:[]) = 0
        counter (a:b:rest) = if and [a == "the", isVowel $ head b]
                              then 1 + counter rest
                              else 0 + counter rest

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

-- 3
countVowels :: String -> Integer
countVowels = sum . map countVowelsInWord . words

countVowelsInWord :: String -> Integer
countVowelsInWord = foldl (\acc next -> if isVowel next then acc + 1 else acc) 0

-- validate the word
newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"
mkWord :: String -> Maybe Word'
mkWord w = if (fromIntegral $ length w) - numVowels < numVowels then Nothing else Just (Word' w)
  where numVowels = countVowelsInWord w

-- it's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = Just (builder n)
  where builder 1 = Succ Zero
        builder x = Succ (builder $ x - 1)

-- small library for maybe
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee x fn (Just y) = fn y

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe fb Nothing = fb
fromMaybe fb (Just x) = x

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes = reverse . foldl (\acc next -> (maybeToList next) ++ acc) []

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = if length y /= length xs
                  then Nothing
                  else Just y
  where y = catMaybes xs

-- small either library
-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> unboxLeft x ++ acc) []
  where unboxLeft (Left a) = [a]
        unboxLeft _ = []

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> unboxRight x ++ acc) []
  where unboxRight (Right b) = [b]
        unboxRight _ = []

-- 3.
-- not sure how this function is designed to work?
-- skipping for now
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = undefined

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\y -> Just (f y))

-- continue on pg 476 for more chapter 12 exercises (skipping at the moment)
