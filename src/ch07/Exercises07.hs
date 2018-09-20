module Exercises07 where

-------------------------------------------
-- Exercises: Grab Bag
-- 1: they're all equivalent
-- 2: b)
-- 3:
--   a)
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  -- where f n = n + 1
  where f = \n -> n + 1

--   b)
addFive x y = (if x > y then y else x) + 5
addFive' = \ x y -> if x > y then y else x + 5

--   c)
mflip f = \x -> \y -> f y x
mflip' f x y = f x y



-------------------------------------------
--  Exercises: Variety Pack

-- 1:
k :: (a, b) -> a
k (x, y) = x

k1 :: Integer
k1 = k ((4-1), 10)

k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Integer
k3 = k (3, True)

--  a) k :: (a, b) -> a
--  b) k2 :: [Char]
--  c) k1, k3

-- 2:

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f') = ((a, d), (c, f'))


-------------------------------------------
--  Exercises: Case Practice

-- 1:
functionC :: Ord a =>  a -> a -> a
functionC x y = case x > y of
  True  -> x
  False -> y

-- 2:
ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

-- 3:
nums :: Integer -> Integer
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

-------------------------------------------
--  Exercises: Artful Dodgy

dodgy :: Integer -> Integer -> Integer
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

-- 1:  dodgy 1 0 = 1 + 0 * 10 = 1
-- 2:  dodgy 1 1 = 1 + 1 * 10 = 11
-- 3:  dodgy 2 2 = 2 + 2 * 10 = 22
-- 4:  dodgy 1 2 = 1 + 2 * 10 = 21
-- 5:  dodgy 2 1 = 2 + 1 * 10 = 12
-- 6:  oneIsOne 1 = 1 + 1 * 10 = 11
-- 7:  oneIsOne 2 = 1 + 2 * 10 = 21
-- 8:  oneIsTwo 1 = 1 + 20 = 21
-- 9:  oneIsTwo 2 = 2 + 20 = 22
-- 10: oneIsOne 3 = 1 + 3 * 10 = 31
-- 11: oneIsTwo 3 = 3 + 20 = 23


-------------------------------------------
-- Exercises: Guard Duty

-- 1:
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  -- | otherwise = 'F'
  | y >= 0.9 ='A'
  | y >= 0.8 ='B'
  | y >= 0.7 ='C'
  | y >= 0.59 = 'D'
  | y < 0.59 ='F'
  where y = x / 100

-- 2: reodering will type check but chnage the functionality as guards does equlaity checks from top to bottom.

-- 3: b)
pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False


-- 4: Eq a => [a
-- 5: pal :: Eq a => [a] -> Bool

-- 6: c)
numbers :: (Num a, Ord a) => a -> a
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

-- 7: (Num a, Ord a) => a
-- 8: (Num a, Ord a) => a -> a



-------------------------------------------
-- Chapter Exercises

-- 1: d)
-- 2: b)
-- 3: d) you've made the polymophic function have Num: apply it to one "numeric" value
-- 4: b)
-- 5: a)


-- Let’s write code

-- 1:

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

--   a)
tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = x `divMod` 10
        (_, d)     = xLast `divMod` 10

--   b) yes
--   c)
hunsD :: Integral a => a -> a
hunsD = tensDigit . (`div` 10)

-- 2:
-- Using a case expression
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y cond =
  case cond of
    True  -> x
    False -> y

-- Using guards
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y cond
  | cond = x
  | otherwise = y

-- 3:
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4:
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5:
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

main = do
  print (roundTrip 4)
  print (id 4)
-- 6:
  print (roundTrip 4 :: Int)
  print (id 4)
