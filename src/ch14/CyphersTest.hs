module CyphersTest where

import Data.Char
import Test.QuickCheck


-- Caesar
caesar :: Int -> String -> String
caesar n = map (\x -> chr $ mod (ord x + n - a) 26 + a)
  where a = ord 'a'

uncaesar :: Int -> String -> String
uncaesar n = caesar (-n)

-- Vigenere
char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char ((char2int c + n) `mod` 26)
          | otherwise = c

vigenere :: String -> String -> String
vigenere ks xs = map (\z -> shift (char2int $ snd z) (fst z)) zs
  where zs = zip xs $ cycle ks

unvigenere :: String -> String -> String
unvigenere ks xs = map (\z -> shift (negate $ char2int $ snd z) (fst z)) zs
  where zs = zip xs $ cycle ks



-------------------------------------------
--  Tests

-- Caesar
genWord :: Gen String
genWord = listOf1 $ elements ['a'..'z']

genShift :: Gen Int
genShift = elements [0..25]

genCaesarParams :: Gen (Int, String)
genCaesarParams = do
  gs <- genShift
  gw <- genWord
  return (gs, gw)

prop_caesar :: Property
prop_caesar = forAll genCaesarParams (\(n, xs) -> uncaesar n (caesar n xs) == xs)

-- Vigenere
genVigenere :: Gen (String, String)
genVigenere = do
  a <- genWord
  b <- genWord
  return (a,b)

prop_vigenere :: Property
prop_vigenere =
  forAll genVigenere (\(ks, xs) -> unvigenere ks (vigenere ks xs) == xs)

runQC :: IO ()
runQC = do
  quickCheck prop_caesar
  quickCheck prop_vigenere
