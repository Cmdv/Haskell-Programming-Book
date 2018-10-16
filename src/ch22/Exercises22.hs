module Exercises22 where

import Data.Char
import Control.Applicative
import Data.Maybe

-------------------------------------------
-- Short Exercise: Warming Up

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev
  return (a, b)

tupled' :: [Char] -> ([Char], [Char])
tupled' =
  rev >>= (\a ->
    cap >>= (\b ->
      return (a, b)))



-------------------------------------------
-- Exercise: Ask
newtype Reader r a = Reader
  { runReader :: r -> a
  }

ask' :: Reader a a
ask' = Reader id


-- Demonstrating the function Applicative
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
      dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address



-------------------------------------------
-- Exercise: Reading Comprehension

-- 1:
myLiftA2 :: Applicative f
         => (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- 2:
asks :: (r -> a) -> Reader r a
asks = Reader

-- 3:
instance Functor (Reader r) where
  fmap f (Reader a) = Reader $ f . a

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  (Reader ra) <*> (Reader rb) = Reader (\x -> ra x (rb x))



-------------------------------------------
-- The Monad of functions

foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+1)

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus :: (Foldable t, Functor t, Num a) => t a -> (t a, Int)
barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r


-------------------------------------------
-- Exercise: Reader Monad

-- 1:
instance Monad (Reader r) where
  return = pure
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

-- 2:
getDogRM' :: Person -> Dog
getDogRM' =
  dogName >>=
    \name -> address >>=
      \addrs -> return $ Dog name addrs


-------------------------------------------
-- Chapter Exercises:
x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys


x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) m m
 where
   m = z' n

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  --
  print $ sequA 7
  print $ foldr (&&) True $ sequA 6
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
