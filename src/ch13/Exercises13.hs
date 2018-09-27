module Exersise13 where

import Control.Monad (forever)
import Data.Char (toLower)
import System.Exit ( exitSuccess )
import System.IO ( BufferMode(NoBuffering)
                 , hSetBuffering
                 , stdout
                 )
-------------------------------------------
-- Intermission: Check your understanding

-- 1: import Control.Monad (forever, when)
-- 2: import Data.Bits , import Database.Blacktip.Types
-- 3: The types for the blacktip library
-- 4:
--   a) MV = Control.Concurrent.MVar, FPC = Filesystem.Path.CurrentOS, CC = Control.Concurrent
--   b) Filesystem
--   c) import Control.Monad (forever, when)

-------------------------------------------
-- Hangman game
-- see hangman/


-------------------------------------------
-- Modifying code

-- 1: see Ciphers.hs
-- 2-3:
palindrome :: IO ()
palindrome = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "Palindrome? "
  input <- getLine
  let line1 = filter (\x -> x `elem` ['a' .. 'z']) $ map toLower input
  if line1 == reverse line1
    then putStrLn "It's a palindrome!"
    else do
      putStrLn "Nope!"
      exitSuccess

-- 4:
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Give me name."
  name <- getLine
  putStrLn "Now, give me an age."
  age <- getLine
  case mkPerson name (read age) of
    Right p -> putStrLn $ "Yay! Successfully got a person. " ++ show p
    Left l -> print l
