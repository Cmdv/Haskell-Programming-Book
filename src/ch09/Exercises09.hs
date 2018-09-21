module Exersise09 where

import Data.Bool
import Data.Char

-------------------------------------------
-- Exercise: EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True  = [False, True]
eftBool True False  = []
eftBool True True   = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ LT = []
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT LT = []
eftOrd GT EQ = []
eftOrd GT GT = [GT]

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftInt (succ a) b

eftChar :: Char -> Char -> String
eftChar a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : eftChar (succ a) b


-------------------------------------------
-- Exercises: Thy Fearful Symmetry

-- 1:
myWords :: String -> [String]
myWords []       = []
myWords (' ':s)  = myWords s
myWords s        = w : myWords t
  where
    w = takeWhile (/= ' ') s
    t = dropWhile (/= ' ') s
-- aka:
-- Prelude.words "sheryl wants fun" == ["sheryl","wants","fun"]

-- 2:
firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen = "Could frame thy fearful symmetry?"

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines []       = []
myLines ('\n':s) = myLines s
myLines s        = w : myLines t
  where
    w = takeWhile (/= '\n') s
    t = dropWhile (/= '\n') s

-- 3:
mySplit :: Char -> String -> [String]
mySplit _ [] = []
mySplit c lst@(x:xs) =
  if c == x
  then mySplit c xs
  else takeWhile w lst : mySplit c (dropWhile w lst)
  where
    w = (/= c)

myWords' :: String -> [String]
myWords' = mySplit ' '

myLines' :: String -> [String]
myLines' = mySplit '\n'

-------------------------------------------
-- Exercises: Square Cube

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

-- 1:
-- [(x, y) | x <- mySqr, y <- myCube]
mySqrCube :: [(Integer, Integer)]
mySqrCube = do
  y <- myCube
  x <- mySqr
  return (x, y)

-- 2:
-- [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
bellow50 :: [(Integer, Integer)]
bellow50 = filter (\(a, b) -> a < 50 && b < 50) mySqrCube

-- 3:
myAllLength :: Int
myAllLength = length bellow50


-------------------------------------------
-- Exercises: Bottom Madness

-- 1: ⊥ undefined will be called as it's the second element of the list
-- 2: fine, take one makes sure just the first element is called
-- 3: ⊥
-- 4: fine
-- 5: ⊥ undefined is now part of the spine
-- 6: fine, take 1 will == [2]
-- 7: ⊥ 1 3 aren't even so undefined evaluated
-- 8: fine returns [1]
-- 9: fine returns [1,3] due to take 2
-- 10: ⊥ take 3 evaluated undefined

-- Intermission: Is it in normal form?

-- 1: NF all values are evaluated
-- [1, 2, 3, 4, 5]

-- 2: WHNF the last ':' hasn't been evaluated
-- 1 : 2 : 3 : 4 : _

-- 3: Neither, last part "enumFromTo" is not a data constructor and it hasn't been fully evaluated.
-- enumFromTo 1 10.

-- 4: Neither, last part of "length" is not a data constructor and it hasn't been fully evaluated.
-- length [1, 2, 3, 4, 5].

-- 5: Neither, last part of "sum" is not a data constructor and it hasn't been fully evaluated.
-- sum (enumFromTo 1 10)

-- 6: Neither, last part of "++" is not a data constructor and it hasn't been fully evaluated.
-- ['a'..'m'] ++ ['n'..'z']

-- 7: WHNF, the last part "," is a data constructor and it hasn't been fully evaluated.
-- (_, 'b')


-------------------------------------------
-- Exercises: More Bottoms

-- 1: ⊥ take 1 evaluates the map and it hits undefined
-- 2: fine, take makes map evaluate just the first list value so it returns [1]
-- 3: ⊥ take 2 evaluates the second value of the list which is undefined
-- 4:
vowels :: String -> [Bool]
vowels = map (`elem` "aeiou")
-- it will return a list of Bool for each Char given if they match a vowel or not

-- 5:
--   a) map (^2) [1..10] == [1,4,9,16,25,36,49,64,81,100]
--   b) map minimum [[1..10], [10..20], [20..30]] == [1,10,20]
--   c) map sum [[1..5], [1..5], [1..5]] == [15,15,15]

-- 6:
foldBool :: (Num b, Eq b) => [b] -> [b]
foldBool = map (\x -> bool x (-x) $ x == 3)


-------------------------------------------
-- Exercises: Filtering

-- 1:
filterMult3 :: [Integer]
filterMult3 = filter (\x -> rem x 3 == 0) [1..30]

-- 2:
getMultLength :: Int
-- getMultLength = length filterMult3
getMultLength = length . filter (\x -> rem x 3 == 0) $ [1..30]

-- 3:

myfilter :: String -> [String]
myfilter xs =
  filter validWords $ words xs
  where
    keywords = ["the", "a", "an"]
    validWords x = notElem x keywords


-------------------------------------------
-- Zipping exercises

-- 1:
myZip :: [a] -> [b] -> [(a,b)]
myZip as bs = do
  a <- as
  b <- bs
  return (a, b)

-- 2:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f as bs = do
  a <- as
  b <- bs
  return $ f a b

-- 3:
myZip2 :: [a] -> [b] -> [(a,b)]
myZip2 = myZipWith (,)


-------------------------------------------
-- Chapter Exercises

-- 1:
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2:
filterLower :: String -> String
filterLower = filter isUpper

-- 3:
capFirst :: String -> String
capFirst []     = []
capFirst (x:xs) = toUpper x : xs


-- 4:
upperAll :: String -> String
upperAll ""     = ""
upperAll (x:xs) = toUpper x : upperAll xs

-- 5 - 6:
capsHead :: String -> Char
-- capsHead s = (toUpper (head s))
-- capsHead s = (toUpper . head) s
capsHead = toUpper . head


-------------------------------------------
-- Ciphers
encode :: Char -> Int
encode ch = ord ch - ord 'a'

decode :: Int -> Char
decode n = chr (ord 'a' + n)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n ch = decode $ mod (f (encode ch) n) 26

rightShift :: Int -> Char -> Char
rightShift = shift (+)

leftShift :: Int -> Char -> Char
leftShift = shift (-)

caesar :: Int -> String -> String
caesar n = map (rightShift n)

unCaesar :: Int -> String -> String
unCaesar n = map (leftShift n)


-------------------------------------------
-- Writing your own standard functions

-- 1:
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- 2:
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

-- 3:
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = x == y || myElem x ys
-- myElem a xs = myAny (== a) xs

-- 4:
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5:
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

-- 6:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7:
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain xs = squishMap id xs

-- 8:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x]       = x
myMaximumBy comp (x:xs) = do
  let y = myMaximumBy comp xs
  case comp x y of
    LT -> y
    EQ -> x
    GT -> x
-- myMaximumBy comp (x:xs) =
--   case comp x (myMaximumBy comp xs) of
--     LT -> x
--     EQ -> x
--     GT -> myMaximumBy comp xs

-- 9:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x]       = x
myMinimumBy comp (x:xs) = do
  let y = myMinimumBy comp xs
  case comp x y of
    LT -> x
    EQ -> x
    GT -> y

-- 10:
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
