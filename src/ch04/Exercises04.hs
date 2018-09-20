module Exercises04 where


-------------------------------------------
-- Mood Swing

data Mood = Blah | Woot deriving Show

-- 1: Mood
-- 2: Blah or Woot
-- 3: The type should be changeMood :: Mood -> Mood
-- 4:

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah


-------------------------------------------
-- Find the Mistakes

-- 1:
-- not True && True

-- 2:
-- not (x == 6)
-- x is not in scope

-- 3:
-- (1 * 2) > 5

-- 4:
-- "Merry" > "Happy"

-- 5:
-- [1, 2, 3] ++ "look at me!"
-- these are of 2 different types [Int] [Char]


-------------------------------------------
-- Chapter Exercises

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]

also :: [String]
also = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, also]

-- 1: length :: [a] -> Int
-- 2:
--   a) 5
--   b) 3
--   c) 2
--   d) 5

-- 3: 6 / 3 = 2.0
--    but 6 / length [1, 2, 3] won't work because length [1, 2, 3] :: Int
--    and `/` needs both arguments to be Fractional

-- 4: div 6 (length [1, 2, 3])

-- 5: 2 + 3 == 5 :: Bool
--    the expected result is True

-- 6: The type is Bool and the expected result is False

-- 7:
-- length allAwesome == 2
-- Works

-- length [1, 'a', 3, 'b']
-- Fail the list contains different types

-- length allAwesome + length awesome == 5
-- Works

-- (8 == 8) && ('b' < 'a') == True && False == False
-- Works

-- (8 == 8) && 9
-- Fails 8 == 8 is a Bool and 9 is an Int

-- 8:
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9:
myAbs :: Integer -> Integer
myAbs n = if n >= 0 then n else negate n

-- 10:
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))


-------------------------------------------
-- Correcting syntax

-- 1:

x' :: Int -> Int -> Int
x' = (+)

f' :: [Int] -> Int
f' xs = w `x'` 1
  where w = length xs

-- using ' with names due to shadowing

-- 2:
myId :: a -> a
myId = \x -> x


-- 3:
myHead :: [a] -> a
myHead = \(x : _) -> x

-- 4:
myFirst :: (a, b) -> a
myFirst (a, _) = a


-------------------------------------------
-- Match the function names to their types

-- 1: c
-- 2: b
-- 3: a
-- 4: d
