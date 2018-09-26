module Exersise12 where

import Control.Applicative
import Data.Maybe (fromMaybe)

type Name = String
type Age = Integer

type ValidatePerson a =
  Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
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
mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)


-------------------------------------------
-- Chapter Exercises

-- 1: id :: a -> a   a is of the kind *
-- 2: r :: a -> f a  the kind of a is * and the kind of f is * -> *

-- String processing

-- 1:
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

notA :: Maybe String -> String
notA = fromMaybe "a"

replaceThe :: String -> String
replaceThe s = unwords $ map (notA . notThe) (words s)


-- 2:
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
  where
    go [] n = 0 + n
    go (w : ws) n | head w `elem` "aeiou" = go ws (1 + n)
                  | otherwise             = go ws n


-- 3:
countVowels :: String -> Int
countVowels s = length $ filter (`elem` "aeiou") s


-- Validate the word

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word =
      let lengthVowels = countVowels word
          lengthCons = length word - lengthVowels
      in
      case lengthVowels > lengthCons of
        True -> Nothing
        False -> Just $ Word' word


-- It’s only Natural
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i | i < 0     = Nothing
               | otherwise = Just $ go i
 where
  go i' | i' == 0   = Zero
        | otherwise = Succ $ go (i' - 1)


-- Small library for Maybe

-- 1:
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

-- 2:
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

-- 3:
fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing  = a
fromMaybe' _ (Just b) = b

-- 4:
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- 5:
catMaybes :: [Maybe a] -> [a]
catMaybes xs = do
  Just x <- xs
  return x

-- 6:
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs | any isNothing xs = Nothing
             | otherwise        = Just (catMaybes xs)

-- Small library for Either

-- 1:
lefts :: [Either a b] -> [a]
lefts = foldr concatLeft []
  where concatLeft (Left a) xs  = a : xs
        concatLeft (Right _) xs = xs

lefts' :: [Either a b] -> [a]
lefts' xs = do
  (Left a) <- xs
  return a

lefts''   :: [Either a b] -> [a]
lefts'' x = [a | Left a <- x]

-- 2:
rights :: [Either a b] -> [b]
rights = foldr concatRight []
  where concatRight (Left _) xs = xs
        concatRight (Right a) xs = a : xs

rights' :: [Either a b] -> [b]
rights' xs = do
  (Right b) <- xs
  return b

-- 3:
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4:
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right a) = Just $ f a

-- 5:
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

-- 6:
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)



-------------------------------------------
-- Write your own iterate and unfoldr

-- 1:
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2:
myUnfoldr ::
    (b -> Maybe (a, b))
  -> b
  -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  Just (a, b') -> a : myUnfoldr f b'

-- 3:
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))


-- Finally something other than a list!
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1:
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (l, m, r) -> Node (unfold f l) m (unfold f r)

-- 2:
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeBuilder 0
  where
    treeBuilder a
      | a < n = Just (a + 1, a, a + 1)
      | otherwise = Nothing
