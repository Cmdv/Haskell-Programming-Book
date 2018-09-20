{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises05 where

-------------------------------------------
-- Type Matching

-- 1:
-- a) c): not :: Bool -> Bool
-- b) d): length :: [a] -> Int
-- c) b): concat :: [[a]] -> [a]
-- d) a): head :: [a] -> a
-- e) e): (<) :: Ord a => a -> a -> Bool


-------------------------------------------
-- Type Arguments

-- 1: a)
-- 2: d)
-- 3: d)
-- 4: c)
-- 5: a)
-- 6: e)
-- 7: f)
-- 8: a)
-- 9: c)


-------------------------------------------
-- Parametricity

-- 1:
id :: a -> a
id x = x
-- impossible sorry

-- 2:
pair :: a -> a -> a
pair x y = x

pair' :: a -> a -> a
pair' x y = y

-- 3:
snd :: a -> b -> b
snd x y = y


-------------------------------------------
-- Apply Yourself

-- 1:
myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"
-- (++) :: [Char] -> [Char] -> [Char]

-- 2:
myMult :: Fractional a => a -> a
myMult x = (x / 3) * 5
-- (*) :: Fractional a => a -> a -> a

-- 3:
myTake :: Int -> [Char]
myTake x = take x "hey you"
-- take :: Int -> [Char] -> [Char]

-- 4:
myCom :: Int -> Bool
myCom x = x > (length [1..10])
-- (>) :: Int -> Int -> Bool


-------------------------------------------
-- Chapter Exersises

-- Multiple choice

-- 1: c)
-- 2: a)
-- 3: b)
-- 4: c)

-- Determine the type

-- 1:
--   a) (* 9) 6 :: Num a => a
--   b) head [(0, "doge"), (1, "kitteh")] :: Num a => (a, [Char])
--   c) head [(0 :: Integer, "doge"), (1, "kitteh")] :: (Integer, [Char])
--   d) if False then True else False :: Bool
--   e) length [1, 2, 3, 4, 5] :: Int
--   f) (length [1, 2, 3, 4]) > (length "TACOCAT") :: Bool

-- 2: w :: Num a => a
-- 3: z :: Num a => a -> a
-- 4: f :: Fractional a => a
-- 5: f :: [Char]

-- Does it compile?

-- 1: bigNum doesn't take an argument potential fixe remove: $ 10
bigNum = (^) 5 $ 10
wahoo = bigNum

-- 2: compiles fine
x' = print
y' = print "woohoo!"
z' = x' "hello world"

-- 3: no c and d were just two Ints so use a to add them together
a = (+)
b = 5
c =  a b 10
d = a c 200

-- 4: no because c' doesn't exist, so it had to be added, using ' to differentiate from declerations above.
a' = 12 + b
b' = 10000 * c'
c' = 99


-- Type variable or specific type constructor

-- 1: Exmaple


-- 2:
-- f :: zed -> Zed -> Blah
--      (a)    (b)    (c)

-- (a) fully polymorphic
-- (b) concrete
-- (c) concrete


-- 3:
--  f :: Enum b => a -> c -> C
--                (a)  (b)  (c)

-- (a) fully polymorphic
-- (b) constrained polymorphic
-- (c) concrete


-- 4:
-- f :: f -> g -> C
--     (a)  (b)  (c)

-- (a) fully polymorphic
-- (b) fully polymorphic
-- (c) concrete


-- Write a type signature

-- 1:
functionH :: [a] -> a
functionH (x:_) = x

-- 2:
functionC :: Ord a => a -> a -> Bool
functionC x y = if x > y then True else False

-- 3:
functionS :: (a, b) -> b
functionS (_, y) = y


-- Given a type, write the function

-- 1:
i :: a -> a
i x = x

-- 2:
cc :: a -> b -> a
cc x _ = x

-- 3:
cc'' :: b -> a -> b
cc'' x _ = x

-- 4:
cc' :: a -> b -> b
cc' _ y = y

-- 5:
r :: [a] -> [a]
r []     = []
r (_:xs) = xs

-- 6:
co :: (b -> c) -> (a -> b) -> a -> c
co g f x = g (f x)

-- 7:
aa :: (a -> c) -> a -> a
aa f x = x

-- 8:
aa' :: (a -> b) -> a -> b
aa' f x = f x


-- Fix it

-- 1:
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
-- 2:
sing' :: [Char]
sing' = if (x < y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

-- 3:
main :: IO ()
main = do
  print $ 1 + 2
  putStrLn $ show 10
  print $ negate (-1)
  print $ (+) 0 blah
    where blah = negate 1


-- Type-Kwon-Do

-- 1:
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g (f x)

-- 2:
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

-- 3:
data X
data Y
data Z
xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4:
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xy ywz x = (fst . ywz) $ xy x
