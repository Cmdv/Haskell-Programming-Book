
module Chapter13.Ciphers where

import           Data.Char
import           System.IO


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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a key: "
  key <- getLine
  putStr "Enter a word: "
  word <- getLine
  putStrLn ("Vigenere   = " ++ vigenere key word)
  putStrLn ("Unvigenere = " ++ unvigenere key (vigenere key word))
