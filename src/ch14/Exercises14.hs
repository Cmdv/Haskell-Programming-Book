module Exercises14 where

import Data.Char
import Data.List (intercalate, sort)
import Test.Hspec
import Test.QuickCheck

-------------------------------------------
-- Intermission: Short Exercise
multMe :: Integral a => a -> a -> a
multMe a 1 = a
multMe a b = a + multMe a (b - 1)

-- main :: IO ()
-- main = hspec $ do
--   describe "Addition" $ do
--     it "15 multiplied with 3 is 45" $ do
--       multMe 15 3 `shouldBe` 45
--     it "2 multilied with 2 is 4" $ do
--      multMe 2 2 `shouldBe` 4


genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

-------------------------------------------
-- Morse code
-- see morse/

-------------------------------------------
-- Validating numbers into words

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
  _ -> undefined

digits :: Int -> [Int]
digits n = go n []
 where
  go n' ds | n' < 10   = n' : ds
           | otherwise = go (n' `div` 10) (n' `mod` 10 : ds)

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord $ digits n)

-- main :: IO ()
-- main = hspec $ do
--   describe "digiToWord" $ do
--     it "returns zero for 0" $
--       digitToWord 0 `shouldBe` "zero"
--     it "returns one for 1" $
--       digitToWord 1 `shouldBe` "one"

--   describe "digits" $ do
--     it "returns [1] for 1" $
--       digits 1 `shouldBe` [1]
--     it "returns [1,0,0] for 100" $
--       digits 100 `shouldBe` [1,0,0]

--   describe "wordNumber" $ do
--     it "one-zero-zero given 100" $
--       wordNumber 100 `shouldBe` "one-zero-zero"
--     it "nine-zero-zero-one for 9001" $
--       wordNumber 9001 `shouldBe` "nine-zero-zero-one"


-------------------------------------------
-- Using QuickCheck

-- 1:
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

-- > quickCheck prop_halfIdentity -- +++ OK, passed 100 tests.

-- 2:
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, _)       = (Just y, x >= y)

prop_sortListOrdered :: (Ord a) => [a] -> Bool
prop_sortListOrdered = listOrdered . sort

qc_sortListOrdered :: IO ()
qc_sortListOrdered = do
  quickCheck (prop_sortListOrdered :: [Int] -> Bool)
  quickCheck (prop_sortListOrdered :: [String] -> Bool)
-- > quickCheck prop_sortListOrdered -- +++ OK, passed 100 tests.


-- 3:
prop_plusAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z
-- quickCheck prop_plusAssociative -- +++ OK, passed 100 tests.

prop_plusCommutative :: (Eq a, Integral a) => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x
-- quickCheck prop_plusCommutative -- +++ OK, passed 100 tests.


-- 4:
prop_multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_multAssociative x y z = x * (y * z) == (x * y) * z

prop_multCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_multCommutative x y = x * y == y * x

qc_mult :: IO ()
qc_mult = do
  quickCheck (prop_multAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_multCommutative :: Int -> Int -> Bool)

-- 5:
prop_quotRem :: (Eq a, Integral a) => a -> a -> Bool
prop_quotRem x y = (quot x y)*y + (rem x y) == x

prop_divMod :: (Eq a, Integral a) => a -> a -> Bool
prop_divMod x y = y == 0 || (div x y) * y + (mod x y) == x

qc_quotRem :: IO ()
qc_quotRem = do
  quickCheck (prop_quotRem :: Int -> Int -> Bool)

qc_divMod :: IO ()
qc_divMod = do
  quickCheck (prop_divMod :: Int -> Int -> Bool)

-- 6:
prop_powerAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_powerAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

prop_powerCommutative :: (Eq a, Integral a) => a -> a -> Bool
prop_powerCommutative x y = x ^ y == y ^ x

qc_powerAssociative :: IO ()
qc_powerAssociative =
  quickCheck (prop_powerAssociative :: Int -> Int -> Int -> Bool)
-- fails for 0's
-- LHS: (0 ^ 0) ^ 0 = 1 ^ 0 = 1
-- RHS: 0 ^ (0 ^ 0) = 0 ^ 1 = 0
-- LHS /= RHS

qc_powerCommutative :: IO ()
qc_powerCommutative = do
  quickCheck (prop_powerCommutative :: Int -> Int -> Bool)
-- fails
-- LHS: 0 ^ 1 = 0
-- RHS: 1 ^ 0 = 1
-- LHS /= RHS

-- 7:
prop_reverseId :: (Eq a) =>  [a] -> Bool
prop_reverseId xs = (reverse . reverse) xs == id xs

qc_reverseId :: IO ()
qc_reverseId = quickCheck (prop_reverseId :: [Char] -> Bool)

-- 8:
prop_funcApplication :: Char -> Bool
prop_funcApplication x = (toLower $ x) == toLower x

prop_funcComposition :: Int -> Bool
prop_funcComposition x = (even . abs) x == (\x' -> even (abs x')) x

-- 9:
prop_foldr1 :: [Int] -> Bool
prop_foldr1 xs = foldr (:) [] xs == (++) [] xs

prop_foldr2 :: [[Int]] -> Bool
prop_foldr2 xs = foldr (++) [] xs == concat xs

-- 10:
prop_lengthTake :: Positive Int -> NonEmptyList Char -> Bool
prop_lengthTake (Positive n) (NonEmpty xs) = length (take n xs) == n

-- 11:
prop_readShow :: Integer -> Bool
prop_readShow x = read (show x) == (x :: Integer)

prop_readShow' :: String -> Bool
prop_readShow' x = read (show x) == (x :: String)


-------------------------------------------
-- Failure

-- This fails because floating point arithmetic introduces precision errors
-- E.g. square (sqrt 5.0) = 5.000000000000001
square :: Double -> Double
square x = x * x

prop_squareIdentity :: Double -> Bool
prop_squareIdentity x = (square . sqrt) x == x



-------------------------------------------
-- Idempotence

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

-- 1:
capitalizeWord :: String -> String
capitalizeWord []       = []
capitalizeWord (c : cs) = toUpper c : cs

prop_idemCap :: String -> Bool
prop_idemCap x =
  (capitalizeWord x == twice capitalizeWord x)
    && (capitalizeWord x == fourTimes capitalizeWord x)

-- 2:
prop_idemSort :: [Int] -> Bool
prop_idemSort xs = (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)


-------------------------------------------
--  Make a Gen random generator for the datatype

-- 1:
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

-- 2:
genFool' :: Gen Fool
genFool' = frequency [(2, return Fulse), (1, return Frue)]


-------------------------------------------
-- Validating ciphers

-- see CyphersTest.hs
