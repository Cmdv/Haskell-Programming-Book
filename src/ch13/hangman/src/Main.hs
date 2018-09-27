module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse, sort)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle =
  Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed incorrect) =
    intersperse ' ' (fmap renderPuzzleChar discovered) ++
    " // Incorrect guesses: " ++ show incorrect ++
    " // Guessed so far: " ++ intersperse ' ' (sort guessed)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in    l >= minWordLength
             && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (minWordLength, maxWordLength)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle x _ _ _) guess = guess `elem` x

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) guess = guess `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just a) = a

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s incorect) c =
  Puzzle word newFilledInSoFar (c : s) newIncorrect
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar

    newFilledInSoFar = zipWith (zipper c) word filledInSoFar
    newIncorrect = if c `elem` word then incorect else incorect + 1

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ incorrect) =
  when (incorrect > 7) $ do
    putStrLn "You lose suckaa!!"
    putStrLn $ "the word was: " ++ wordToGuess
    exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  when (all isJust filledInSoFar) $
  do
    putStrLn "You win!"
    exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn "Your guess must be a single character!!"

main :: IO ()
main = do
  putStrLn "Let's play Hangman - you get up to 7 wrong guesses!"
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
