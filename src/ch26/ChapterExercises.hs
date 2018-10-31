module ChapterExercises where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-------------------------------------------
-- Chapter Exercises:

-- 1:
rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> pure (r - 1)

-- 2:
rDec' :: Num a => Reader a a
rDec' = ReaderT $ pure . subtract 1

-- 3:
-- 4:
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ return . show

-- 5:
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT $ \x -> do
    putStrLn ("Hi: " <> show x)
    return (x + 1)

-- 6:
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum =
  StateT $ \x -> do
    putStrLn ("Hi: " <> show x)
    return (show x, x + 1)

-- 7:
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v


doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
      Nothing -> putStrLn "MOAR EXCITE"
      Just e -> putStrLn ("Good, was very excite: " ++ e)
