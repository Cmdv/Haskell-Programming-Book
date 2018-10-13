module Exercises18 where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-------------------------------------------
-- bind

bind' :: Monad m => (a -> m b) -> m a -> m b
bind' f xs = join $ fmap f xs


-- Using the Maybe Monad
data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmty :: String -> Maybe String
noEmty ""  = Nothing
noEmty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing


weightCheck :: Cow -> Maybe Cow
weightCheck c = do
  let w = weight c
      n = name c
  if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nameC <- noEmty name'
  ageC <- noNegative age'
  weightC <- noNegative weight'
  weightCheck (Cow nameC ageC weightC)

--

type Founded = Int

type Coders = Int

data SoftwareShop =
  Shop {
      founded :: Founded
    , programmers :: Coders
       } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 100 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded' <- validateFounded years
  programmers' <- validateCoders coders
  if programmers' > div founded' 10
    then Left $ TooManyCodersForYears founded' programmers'
    else Right $ Shop founded' programmers'


-------------------------------------------
-- Short Exercise: Either Monad

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

instance Monoid a => Applicative (Sum a) where
  pure = Second
  (<*>) (First x) (First y)   = First (x <> y)
  (<*>) (Second _) (First y)  = First y
  (<*>) (First x) (Second _)  = First x
  (<*>) (Second f) (Second y) = Second (f y)


-------------------------------------------
data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  (<*>) (CountMe n f) (CountMe n' a) = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

-- main = do
--   let trigger :: CountMe (Int, String, Int)
--       trigger = undefined
--   quickBatch $ functor trigger
--   quickBatch $ applicative trigger
--   quickBatch $ monad trigger

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = g a >>= f


-------------------------------------------
-- Chapter Exercises:

-- 1:
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg


-- 2:
data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' x) = Left' (f x)
  fmap _ (Right' x) = Right' x

instance Monoid b => Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Right' a) (Left' _)  = Right' a
  (<*>) (Left' _) (Right' a)  = Right' a
  (<*>) (Left' f) (Left' a)   = Left' (f a)
  (<*>) (Right' a) (Right' b) = Right' (a <> b)

instance Monoid b => Monad (PhhhbbtttEither b) where
  return = pure
  Left' y >>= f = f y
  Right' y >>= _ = Right' y


-- 3:
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity a) (Identity b) = Identity (a b)

instance Monad Identity where
  Identity a >>= f = f a


-- 4:
data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Semigroup (List a) where
  (<>) = mappend

instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Applicative List where
  pure x         = Cons x Nil
  _ <*> Nil      = Nil
  Nil <*> _      = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)


instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a Nil) f = f a
  (>>=) (Cons a rest) f = f a <> (rest >>= f)


-------------------------------------------
-- Chapter Exercises 2:

-- 1:
j :: Monad m => m (m a) -> m a
j = join


-- 2:
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap


-- 3:
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2


-- 4:
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap


-- 5:
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  fx <- f x
  fxs <- meh xs f
  return $ fx : fxs

-- 6:
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
