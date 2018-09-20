module Exercises08 where

import Data.List (intercalate, map)

-------------------------------------------
-- Intermission: Exercise

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

-- applyTimes 5 (+1) 5

-- = (+1) . applyTimes 4 (+1) $ 5

-- = (+1) (applyTimes 4 (+1) 5)

-- = (+1) $ applyTimes 4 (+1) 5

-- = (+1) $ (+1) $ applyTimes 3 (+1) 5

-- = (+1) $ (+1) $ (+1) $ applyTimes 2 (+1) 5

-- = (+1) $ (+1) $ (+1) $ (+1) $ applyTimes 1 (+1) 5

-- = (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ applyTimes 0 (+1) 5

-- = (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ 5

-- = (+1) . (+1) . (+1) . (+1) . (+1) $ 5



-------------------------------------------
-- Chapter Exercises

-- Review of types

-- 1: d)
-- 2: b)
-- 3: d)
-- 4: b)
func :: [a] -> [a] -> [a]
func x y = x ++ y

-- func "Hello" "World" = "HelloWorld"
--         [a]     [a]        [a]


-- Review of currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1: appedCatty "woohoo!" == "wooops mrow woohoo!"
-- 2: frappe "1" == "1 mrow haha"
-- 3: frappe (appedCatty "2") == "woops mrow 2 mrow haha"
-- 4: appedCatty (frappe "blue") == "woops mrow blue mrow haha"
-- 5:
--   cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
-- == "pink mrow haha mrow green mrow woops mrow blue"
-- 6: cattyConny (flippy "Pugs" "are") "awesome" == "are mrow Pugs mrow awesome"


-- Recursion

-- 1:
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

-- dividedBy 15 2
-- = go 15 2 0
-- = go 13 2 1
-- = go 11 2 2
-- = go  9 2 3
-- = go  7 2 4
-- = go  5 2 5
-- = go  3 2 6
-- = go  1 2 7
-- = (7, 1)

-- 2:
sumMe :: (Eq a, Num a) => a -> a
sumMe 1 = 1
sumMe n = n + sumMe (n - 1)

-- 3:
multMe :: Integral a => a -> a -> a
multMe a 1 = a
multMe a b = a + (multMe a (b - 1))


-- Fixing dividedBy

data DividedResult =
    Result Integer
  | DividedByZero

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' _ 0       = DividedByZero
dividedBy' num denom = go (abs num) (abs denom) 0
  where
    go n d x
      | num < 0 && denom < 0 && n < d = Result x
      | num > 0 && denom > 0 && n < d = Result x
      | num < 0 && denom > 0 && n < d = Result (negate x)
      | num > 0 && denom < 0 && n < d = Result (negate x)
      | otherwise = go (n - d) d (x + 1)


-- McCarthy 91 function

mc91 :: (Num a, Ord a) => a -> a
mc91 x
  | x > 100   = x - 10
  | otherwise = mc91 $ mc91 $ x + 11



-- Numbers into words

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "zero"

digits :: Int -> [Int]
digits n
  | n < 10    = [ mod n 10 ]
  | otherwise = digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
