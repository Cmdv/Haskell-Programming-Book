module Exercises17 where

import Control.Applicative
import Data.List

import Data.Monoid
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-------------------------------------------
-- Exercises: Lookups

-- added Interger constraints on these fuctions due to compiler warnings
-- 1:
added :: Maybe Integer
added =
  (+3) <$> lookup (3 :: Integer) (zip [1, 2, 3] [4, 5, 6])

-- 2:
y2 :: Maybe Integer
y2 = lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

z2 :: Maybe Integer
z2 = lookup (2 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y2 <*> z2

-- 3:
x3 :: Maybe Int
x3 = elemIndex (3 :: Integer) [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex (4 :: Integer) [1, 2, 3, 4, 5]

max':: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x3 <*> y3

-- 4:
xs4 :: [Integer]
xs4 = [1, 2, 3]

ys4 :: [Integer]
ys4 = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs4 ys4

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs4 ys4

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> x4 <*> y4)


-------------------------------------------
-- Exercise: Identity Instance

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a ) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)



-------------------------------------------
-- Exercise: Constant Instance

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant f) (Constant x) = Constant $ f <> x


-------------------------------------------
-- Exercise: Fixer Upper

-- 1:
ex1 :: Maybe String
ex1 = const <$> Just "Hello" <*> pure "World"

-- 2:
ex2 :: Maybe (Integer, Integer, String, [Integer])
ex2 = (,,,) <$> Just 90
            <*> Just 10
            <*> Just "Tierness"
            <*> pure [1, 2, 3]


-------------------------------------------
-- List Applicative Exercise

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

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' $ fmap f as

instance Eq a => EqProp (List a) where
    (=-=) = eq

-------------------------------------------
-- ZipList Applicative Exercise

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' 1 (Cons x _)  = Cons x Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = xs
  where xs = Cons x xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _                   = Nil
zipWith' _ _ Nil                   = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Applicative ZipList' where
  pure x                          = ZipList' $ repeat' x
  _ <*> (ZipList' Nil)            = ZipList' Nil
  (ZipList' Nil) <*> _            = ZipList' Nil
  (ZipList' xs) <*> (ZipList' ys) = ZipList' (zipWith' id xs ys)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary


-------------------------------------------
-- Exercise: Variations on Either

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure                           = Success
  (<*>) (Failure e) (Failure e') = Failure (e <> e')
  (<*>) (Failure e) _            = Failure e
  (<*>) _ (Failure e)            = Failure e
  (<*>) (Success f) (Success a)  = Success (f a)


-------------------------------------------
-- Chapter Exercises

-- 1:
-- pure :: Applicative f => a -> f a
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- pure :: a -> [] a
-- (<*>) :: [] (a -> b) -> [] a -> [] b

-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- Need the monoid restriction on the first variable 'a'
-- pure :: Monoid a => a -> (,) a a
-- (<*>) :: Monoid a => (,) a (a -> b) -> (,) a a -> (,) a b

-- No need for monoid restriction; but use 'e' as first type variable
-- pure :: a -> (->) e a
-- (<*>) :: (->) a (a -> b) -> (->) a a -> (->) a b

-------------------------------------------
-- Write instances for the following datatypes

-- 1:
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x                      = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq



-- 2:
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two x g) (Two y z) = Two (mappend x y) (g z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq


-- 3:
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three x y f) <*> (Three z1 z2' z3) =
          Three (mappend x z1) (mappend y z2') (f z3)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
      Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- 4:
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' x f g) <*> (Three' z1 z2' z3) = Three' (mappend x z1) (f z2') (g z3)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq



-- 5:
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x1 x2 x3 x) = Four x1 x2 x3 (f x)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four x1 x2 x3' f) <*> (Four y1 y2' y3' y) =
        Four (mappend x1 y1) (mappend x2 y2') (mappend x3' y3') (f y)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6:
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 x) = Four' x1 x2 x3 (f x)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' x1 x2 x3' f) <*> (Four' y1 y2' y3' y) =
        Four' (mappend x1 y1) (mappend x2 y2') (mappend x3' y3') (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos xs ys zs = (,,) <$> xs <*> ys <*> zs

main :: IO ()
main = do
  -- quickBatch $ applicative (ZipList' (Cons ("a", "b", 1 :: Int) Nil))
  quickBatch $ applicative (undefined :: Pair (Int, Double, String))
  quickBatch $ applicative (undefined :: Two String (Int, Double, String))
  quickBatch $ applicative (undefined :: Three String String (Int, Double, String))
  quickBatch $ applicative (undefined :: Three' String (Int, Double, String))
  quickBatch $ applicative (undefined :: Four [Int] String [Int] (Int, Double, String))
  quickBatch $ applicative (undefined :: Four' [Int] (Int, Double, String))
