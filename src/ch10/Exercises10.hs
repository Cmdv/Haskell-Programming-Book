module Exersise10 where

import Data.Time

-------------------------------------------
-- Exercises: Understanding Folds

-- 1: b)
-- 2:
--     foldl (flip (*)) 1 [1..3]
--   = foldl (flip (*)) ((flip (*)) 1 1) [2, 3]
--   = foldl (flip (*)) ((flip (*)) 1 2) [3]
--   = foldl (flip (*)) ((flip (*)) 2 3) []
--   = foldl (flip (*)) 6 []
--   = 6

-- 3: c)
-- 4: a)
-- 5:
--   a): foldr (++) "" ["woot", "WOOT", "woot"]
--   b): foldr max '\NUL' "fear is the little death"
--       foldr max (minBound :: Char) "fear is the little death"
--   c): foldr (&&) True [False, True]
--   d): foldr (||) True [False, True]
--       no mater what Bool list you give it, it will always return True
--       unless you change the second argument to False
--   e): foldl (flip $ (++) . show) "" [1..5]
--   f): foldr (flip const) 'a' [1..5]
--   g): foldr (flip const) 0 "tacos"
--   h): foldl (const) 0 "burritos"
--   i):  foldl (const) 'z' [1..5]


-------------------------------------------
-- Exercises: Database Processing

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
  ]

-- 1:
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = db >>= (\(DbDate a) -> return a)

-- 2:
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = db >>= (\(DbNumber a) -> return a)

-- 3:
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4:
subDb :: [DatabaseItem] -> Integer
subDb = sum . filterDbNumber

-- 5:
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral . flip div (length db). fromIntegral . subDb $ db



-------------------------------------------
-- Scans Exercises

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- 1:
twentyFibs :: [Integer]
twentyFibs = take 20 fibs

-- 2:
lessThanHundredFibs :: [Integer]
lessThanHundredFibs = takeWhile (< 100) fibs

-- 3:
scanFactorial :: [Integer]
scanFactorial = scanl (*) 1 [2..]


-------------------------------------------
-- Chapter Exercises

-- Warm-up and review

-- 1:
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

--   a)
tripples :: [(Char, Char, Char)]
tripples = do
  s <- stops
  v <- vowels
  s' <- stops
  return (s', v, s)

--   b:
justAP :: [(Char, Char, Char)]
justAP = filter (\(a,_,_) -> a `elem` "p") tripples

--   c)
nouns :: [String]
nouns = ["comic", "research", "light", "hair"]

verbs :: [String]
verbs = ["recognise", "throwing", "poop"]

mumbleJumble :: [(String, String, String)]
mumbleJumble = do
  n <- nouns
  v <- verbs
  n' <- nouns
  return (n,v,n')


-- 2: gives you the average word length of a sentence
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
           (length (words x))

-- 3:
seekritFunc' :: String -> Double
seekritFunc' x = do
  let wordz = words x
      totalLength = sum $ map length wordz
      numWords = length wordz
  fromIntegral totalLength / fromIntegral numWords


-- Rewriting functions using folds
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- 1:
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2:
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- 3:
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (== x)) False
-- myElem = myAny (== x)

-- 4:
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


-- 5:
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6:
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\b a -> if f b == True then b : a else a) []

-- 7:
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9:
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (last xs) xs

-- 11:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs
