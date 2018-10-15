module Exercises20 where

import Data.Foldable
import Data.Functor
import Data.Monoid


data Identity a = Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-------------------------------------------
-- Exercises: Library Functions

-- 1:
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2:
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3:
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' val xs = getAny $ foldMap (\x -> Any $ x == val) xs

-- 4:
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where
    f :: Ord a => a -> Maybe a -> Maybe a
    f a ma = case fmap (a <=) ma of
               Just True  -> Just a
               Just False -> ma
               Nothing    -> Just a

-- 5:
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where
    f :: Ord a => a -> Maybe a -> Maybe a
    f a ma = case fmap (a >=) ma of
               Just True  -> Just a
               Just False -> ma
               Nothing    -> Just a

-- 6:
null' :: (Foldable t) => t a -> Bool
null' = foldr f True
  where
    f :: a -> Bool -> Bool
    f _ _ = False

-- 7:
length' :: (Foldable t) => t a -> Int
length' xs = getSum $ foldMap (\_ -> Sum 1) xs

-- 8:
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9:
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10:
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr g mempty
  where
    g a m = f a <> m


-------------------------------------------
-- Chapter Exercises:

-- 1:
data Constant a b =
  Constant b
  deriving (Show, Eq)

instance Foldable (Constant a) where
  foldr _ x _ = x


-- 2:
data Two a b =
  Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b


-- 3:
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c ) = f c


-- 4:
data Three' a b =
  Three' a b b
  deriving (Show, Eq)

instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = f y <> f z


-- 5:
data Four' a b =
  Four' a b b b
  deriving Show

instance Foldable (Four' a) where
  foldr f z (Four' _ b0 b1 b2) = foldr f z [b0, b1, b2]


-- 6:
filterF :: ( Applicative f , Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap g
  where
    g x = if f x
          then pure x
          else mempty
