module Exercises06 where

import Data.List (sort)
-------------------------------------------
-- Eq Instances

-- 1:
data TisAnInterger = TisAn Integer

instance Eq TisAnInterger where
  (==) (TisAn a) (TisAn b) = a == b


-- 2:
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'


-- 3:
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b)     = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (== ) _ _                          = False


-- 4:
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = a == c && b == d


-- 5:
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) = a == c && b == d


-- 6:
data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _                     = False


-- 7:
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b)     = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _                     = False


-------------------------------------------
-- Tuple Experiment

quotRem' :: Integral a => a -> a -> (a, a)
quotRem' xx y = (xx `quot` y, xx `rem` y)

divMod' :: Integral a => a -> a -> (a, a)
divMod' xx y = (xx `div` y, xx `mod` y)


-------------------------------------------
-- Will They Work?

-- 1: yes it will
-- max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
-- max 3 5
-- 5

-- 2: yes it will
-- compare (3 * 4) (3 * 5)
-- compare 12 15
-- LT

-- 3: No it won't "Julie" & True aren't the same type
-- compare "Julie" True

-- 4: yes it will
-- (5 + 3) > (3 + 6)
-- 8 > 9
-- False


-------------------------------------------
-- Chapter Exersises

-- Multiple choice

-- 1: c)
-- 2: b)
-- 3: a)
-- 4: c)
-- 5: a)

-- Does it typecheck?

-- 1: No it doesn't have a Show instance for Person
data Person = Person Bool
  deriving (Show) -- new

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2: No it's missing Eq instance for Mood
data Mood = Blah | Woot
  deriving (Show, Eq) -- new

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

-- 3:
--   a) Mood
--   b) Error 9 is not of type Mood
--   c) Error no instance of Ord for Mood

-- 4: Yes
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"


-- Given a datatype declaration, what can we do?

data Rocks = Rocks String
  deriving (Eq, Show, Ord) -- new

data Yeah = Yeah Bool
  deriving (Eq, Show, Ord) -- new

data Papu = Papu Rocks Yeah
  deriving (Eq, Show, Ord) -- new

-- 1: No
-- phew = Papu "chases" True
phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)

-- 2: Yes
truth :: Papu
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3: Yes
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4: No Papa, Yeah, Rocks were missing Ord instances
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'


-- Match the types

-- 1: No
i :: Num a => a
-- i :: a
i = 1

-- 2: No Num isn't a superclass to Fractional
fl :: Fractional a => a
-- fl :: Num a => a
fl = 1.0

-- 3: Yes this works
-- fl' :: Float
fl' :: Fractional a => a
fl' = 1.0

-- 4: Yes this works
-- fl'' :: Float
fl'' :: RealFrac a => a
fl'' = 1.0

-- 5: Yes this works
-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- 6: Yes this works
-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- 7: No type a is too general
myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund x = myX

-- 8: No the return type must be Int
sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
sigmund' x = myX

-- 9: Yes this works
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)
-- jung = minimum -- alternate solution

-- 10: Yes this works
--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11: No the return type needs to be Char
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)


-- Type-Kwon-Do Two: Electric Typealoo

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = f a + fromInteger n
