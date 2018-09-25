{-# LANGUAGE FlexibleInstances #-}

module Exersise11 where

import Data.Char
import Data.Int
import Data.List


-------------------------------------------
-- Exercises: Dog Types
data DogueDeBordeaux doge =
  DogueDeBordeaux doge

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-- 1: Doogies is a type constructor
-- 2: Doggies => * -> *
-- 3: String => *
-- 4: Doggies Integer
-- 5: Doggies Integer
-- 6: Doogie String
-- 7: DogueDeBordeaux is both a type constructor and data constructor as the same name is used for both.
-- 8: DogueDeBordeaux => doge -> DogueDeBordeaux doge
-- 9: DogueDeBordeaux String


-------------------------------------------
-- Exercises: Vehicles

data Price = Price Integer deriving (Eq, Show)

--ex5: data Size = Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  | None
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline
  --ex5: | Plane Airline Size
  deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir
--ex5: doge = Plane PapuAir (Size 5000)

-- 1: Vehicle
-- 2:
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
--ex5: isPlane (Plane _ _ ) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3-4:
getManu :: Vehicle -> Manufacturer
getManu (Car  a _) = a
getManu (Plane _)  = None

-- 5: see all ex5:


-------------------------------------------
-- Exercises: Cardinality

-- 1: 1
-- 2: 3

-- 3:
-- (abs (fromIntegral (maxBound :: Int16))) + (fromIntegral (minBound :: Int16)) + 1
-- 32768 + 32767 + 1
-- 65536

-- 4:
-- minBound :: Int == -9223372036854775808
-- maxBound :: Int == 9223372036854775807
-- minBound :: Integer == no instance for (Bounded Integer)
-- maxBound :: Integer == no instance for (Bounded Integer)

-- 5: 2^8 == 256


-------------------------------------------
-- Exercises: For Example

data Example = MakeExample deriving Show

-- 1: :t MakeExample => Example
--    :t Example => error: Data constructor not in scope: Example (this is because it is a data constructor)
-- 2: :i Example => data Example = MakeExample ... instance [safe] Show Example
-- 3:

data MyExample = MakeMyExample Int deriving Show
-- :t MakeMyExample :: Int -> MyExample


-------------------------------------------
-- Exercises: Logic Goats

class TooMany a where
  tooMany :: a -> Bool

-- 1:
newtype Pair = Pair (Int, String) deriving (Show, Eq)

instance TooMany Pair where
  tooMany(Pair(n, _)) = n > 42

-- 2:
newtype SumSheep = SumSheep (Int, Int) deriving Show

instance TooMany SumSheep where
  tooMany (SumSheep (m, n)) = m + n > 42

-- 3:
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany $ x + y


-------------------------------------------
-- Exercises: Pity the Bool

data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)
-- 1: 2 + 2 = 4

-- 2:
data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)
-- Its cardinality is 256 + 2 = 258.

myTooSmallNumba :: NumberOrBool
myTooSmallNumba = Numba (-200)
--  Literal -200 is out of the Int8 range -128..127

myTooBigNumba :: NumberOrBool
myTooBigNumba = Numba (200)
--  Literal 200 is out of the Int8 range -128..127

-------------------------------------------
-- Exercises: How Does Your Garden Grow?

-- 1:
-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show

type Gardener = String

-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show


-------------------------------------------
-- Exercise: Programmers
data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = do
  os' <- allOperatingSystems
  lang' <- allLanguages
  pure Programmer {os = os', lang = lang'}


-------------------------------------------
-- Exercises: The Quad

data Quad = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- 1: 4 + 4 = 8 different forms for sum
eQuad :: Either Quad Quad
eQuad = undefined

-- 2: 4 * 4 = 16 different forms for product
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- 3: 4 ^ 4 = 256 different forms
funcQuad :: Quad -> Quad
funcQuad = undefined

-- 4: 2 * 2 * 2 = 8
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- 5: 2 ^ 2 ^ 2 = 16
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- 6:
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
-- (Bool, Quad) -> Quad
--  4^(2*4) = 65536



-------------------------------------------
-- Write map for BinaryTree

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay = if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test faied"

-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = preorder left ++ [x] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = preorder left ++ preorder right ++ [x]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2
  (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

-- Write foldr for BinaryTree

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ x Leaf = x
foldTree f x (Node left y right) = f y (foldTree f (foldTree f x left) right)


-------------------------------------------
-- Chapter Exercises

-- Multiple choice
-- 1: a)
-- 2: c)
-- 3: b)
-- 4: c)

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

vigenere :: String -> String -> String
vigenere keyword = zipWith (rightShift . encode) (concat $ repeat keyword)

unvigenere :: String -> String -> String
unvigenere keyword = zipWith (leftShift . encode) (concat $ repeat keyword)


-- As-patterns

-- 1:
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xx@(x:xs) yy@(_:ys) = if x `elem` yy
  then isSubseqOf xs ys
  else isSubseqOf xx ys

-- 2:
capitalizeWords :: String -> [(String, String)]
capitalizeWords sen = do
  wrds <- words sen
  return (wrds, capitalizeWord wrds)


-- Language exercises

-- 1:
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

-- 2:
capitalizeParagraph :: String -> String
capitalizeParagraph = go True
  where
    go _ [] = []
    go b (' ':cs) = ' ' : go b cs
    go _ ('.':cs) = '.' : go True cs
    go False (c:cs)  = c : go False cs
    go True (c:cs)   = toUpper c : go False cs


-- Phone exercise
-- --------------------------
-- |   1   | 2 ABC | 3 DEF  |
-- --------------------------
-- | 4 GHI | 5 JKL | 6 MNO  |
-- --------------------------
-- | 7 PQRS| 8 TUV | 9 WXYZ |
-- --------------------------
-- |   *^  | 0 +_  |  # .,  |
-- --------------------------
-- 1:
data DaPhone = DaPhone [(Int, String)] deriving Show

stdPhone :: DaPhone
stdPhone = DaPhone [
    (0,"+ 0"),
    (1,"1"),
    (2,"ABC2"),
    (3,"DEF3"),
    (4,"GHI4"),
    (5,"JKL5"),
    (6,"MNO6"),
    (7,"PQRS7"),
    (8,"TUV8"),
    (9,"WXYZ9"),
    (10,"^*"),
    (11,".,#")
  ]


convo :: [String]
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

-- 2:
-- validButtons = "1234567890*#"
type Digit = Int

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c
  | toUpper c == c = keyPhone p '^' : keyPhone p c : []
  | otherwise = [keyPhone p (toUpper c)]

-- assuming the default phone definition
-- 'a' -> ('2', 1)
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead (DaPhone []) _ = []
cellPhonesDead _ [] = []
cellPhonesDead p (x:xs) = reverseTaps p x ++ cellPhonesDead p xs

keyPhone :: DaPhone-> Char -> (Digit, Presses)
keyPhone (DaPhone p) c = (key, presses)
  where
    foundIn = filter (\(_, vals) -> elem c vals) p
    (key, values) = head foundIn
    presses = (+1) $ head $ elemIndices c values

fingerTaps :: DaPhone -> String -> Presses
fingerTaps p s = foldr
              (\(_, presses) acc -> acc + presses) 0
              $ cellPhonesDead p s


sortedLetters :: String -> [(Char, Int)]
sortedLetters xs = map (\letter@(x:_) -> (x, length letter)) (group $ sort xs)

mostUsedLetter :: String -> Char
mostUsedLetter xs = fst $ head $ sortedLetters xs

lettersCost :: DaPhone -> String -> [(Char, Presses)]
lettersCost p s = map
  (\(c, n) -> (c, (*) n $ sum $ map snd $ reverseTaps p c))
  (sortedLetters s)


-- Hutton’s Razor

-- 1:
data Expr
       = Lit Integer
       | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n)   = n
eval (Add a b) = eval a + eval b

-- 2:
printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
