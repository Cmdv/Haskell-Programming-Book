module Exercises23 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import qualified Data.DList as DL
import System.Random


-- RandomExample
data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

-- rollDie :: State StdGen Die
-- rollDie = state $ do
--   (n, s) <- randomR (1, 6)
--   return (intToDie n, s)

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

-- rollDieThreeTimes :: (Die, Die, Die)
-- rollDieThreeTimes = do
--   let s = mkStdGen 0
--       (d1, s1) = randomR (1, 6) s
--       (d2, s2) = randomR (1, 6) s1
--       (d3, _)  = randomR (1, 6) s2
--   (intToDie d1, intToDie d2, intToDie d3)

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sm count gen
      | sm >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sm + die) (count + 1) nextGen

-------------------------------------------
-- Exercises: Roll Your Own

-- 1:
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sm count gen
      | sm >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sm + die) (count + 1) nextGen

-- 2:
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sm count dies gen
      | sm >= n = (count, dies)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sm + die) (count + 1) (dies ++ [intToDie die]) nextGen


-------------------------------------------
-- Write State for yourself

newtype Moi s a =
  Moi {runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  -- fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi $ \s -> let (ga, gs) = g s
                in (f ga, gs)

instance Applicative (Moi s) where
  pure a = Moi (\s -> (a, s))
  (Moi f) <*> (Moi g) =
    Moi $ \s -> let (h, s')  = f s
                    (i, s'') = g s'
                in (h i, s'')

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g =
    Moi $ \s -> let (a, s') = f s
                    (b, s'') = runMoi (g a) s'
                in  (b, s'')

-------------------------------------------
-- FixzzBuzz:
-- with reverse
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResults list) []

addResults :: Integer -> State [String] ()
addResults n = do
  xs <- get
  let results = fizzBuzz n
  put (results : xs)

-- main :: IO ()
-- main = mapM_ putStr $ reverse $ fizzBuzzList [1..100]

-- alternate solution

fizzBuzzList' :: [Integer] -> [String]
fizzBuzzList' list =
  let dlist = execState (mapM_ addResult list) DL.empty
  in DL.apply dlist []

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

-- main :: IO ()
-- main = mapM_ putStrLn $ fizzBuzzList' [1..100]

-- clean up
fizzBuzzList'' :: [Integer] -> DL.DList String
fizzBuzzList'' list = execState (mapM_ addResult list) DL.empty


addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

-- main :: IO ()
-- main = mapM_ putStrLn $ fizzBuzzList'' [1..100]

fizzbuzzFromTo :: Integer -> Integer -> DL.DList String
fizzbuzzFromTo i j = fizzBuzzList'' [j, (j-1) .. i]


-------------------------------------------
-- Chapter exercises:

-- 1:
get' :: Moi s s
get' = Moi $ \s -> (s, s)

-- 2:
put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

-- 3:
exec' :: Moi s a -> s -> s
exec' (Moi sa) s = snd $ sa s

-- 4:
eval' :: Moi s a -> s -> a
eval' (Moi sa) = fst . sa

-- 5:
modify' :: (s -> s) -> Moi s ()
modify' ss = Moi $ \s -> ((), ss s)
