{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercises16 where

import Test.QuickCheck
import Test.QuickCheck.Function

-------------------------------------------
-- Exercises: Be Kind

-- 1: *
-- 2: b = * -> *  T = * -> *
-- 3: c = * -> * -> *


-------------------------------------------
-- Exercises: Heavy Lifting
-- sometimes going to use the infix operator for fmap = <$>

-- 1:
a' :: [Int]
a' = (+1) <$> read "[1]" :: [Int]


-- 2:
b' :: Maybe [String]
b' = (fmap . fmap) (++ "lol") (Just ["Hi", "Hello"])


-- 3:
c' :: Integer -> Integer
c' = fmap (*2) (\x -> x - 2)


-- 4:
d' :: Int -> String
d' = fmap ((return '1' ++) . show) (\x -> [x, 1..3])


-- 5:
e' :: IO Integer
e' = let ioi = readIO "1" :: IO Integer
         changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed



-------------------------------------------
-- Exercises: Instances of Func

prop_FunctorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
prop_FunctorIdentity f = fmap id f == f

prop_FunctorCompose :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
prop_FunctorCompose (Fun _ f) (Fun _ g) x = fmap g (fmap f x) == fmap (g . f) x

type IntToInt = Fun Int Int

type IntFC = [Int] -> IntToInt -> IntToInt -> Bool


-- 1:
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityTest :: IO ()
identityTest = do
  quickCheck $ \x -> prop_FunctorIdentity (x :: Identity Int)
  quickCheck $ \f g x -> prop_FunctorCompose (f :: IntToInt) (g :: IntToInt) (x :: Identity Int)


-- 2:
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

pairGen :: Arbitrary a => Gen (Pair a)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = pairGen

pairTest :: IO ()
pairTest = do
  quickCheck $ \x -> prop_FunctorIdentity (x :: Pair Int)
  quickCheck $ \f g x -> prop_FunctorCompose (f :: IntToInt) (g :: IntToInt) (x :: Pair Int)

-- 3:
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

twoTest :: IO ()
twoTest = do
  quickCheck $ \x -> prop_FunctorIdentity (x :: Two Int Int)
  quickCheck $ \f g x -> prop_FunctorCompose (f :: IntToInt) (g :: IntToInt) (x :: Two Int Int)


-- 4:
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c)
         => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
         => Arbitrary (Three a b c) where
  arbitrary = threeGen

threeTest :: IO ()
threeTest = do
  quickCheck $ \x -> prop_FunctorIdentity (x :: Three Int Int Int)
  quickCheck $ \f g x -> prop_FunctorCompose (f :: IntToInt) (g :: IntToInt) (x :: Three Int Int Int)


-- 5:
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
 arbitrary = do
  a <- arbitrary
  b <- arbitrary
  return (Three' a b b)

three'Test :: IO ()
three'Test = do
  quickCheck $ \x -> prop_FunctorIdentity (x :: Three' Int Int)
  quickCheck $ \f g x -> prop_FunctorCompose (f :: IntToInt) (g :: IntToInt) (x :: Three' Int Int)


-- 6:
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (Four a b c d) where
 arbitrary = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

fourTest :: IO ()
fourTest = do
  quickCheck $ \x -> prop_FunctorIdentity (x :: Four Int Int Int Int)
  quickCheck $ \f g x -> prop_FunctorCompose (f :: IntToInt) (g :: IntToInt) (x :: Four Int Int Int Int)


-- 8:
-- no because it doesn't have the right kind it needs a * -> * but it's just *


-------------------------------------------
-- Exercises: Possibly

data Possibly a =
    LolNope
  | Yepper a
  deriving (Show, Eq)

instance Functor Possibly where
  fmap _ LolNope    = LolNope
  fmap f (Yepper a) = Yepper (f a)



-------------------------------------------
-- Short Exercise

-- 1:
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- 2: The Fuctor requires Sum to be of Kind * -> *
--    and to do so you have to use `Sum a` which only leaves us the `b` to fmap

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)



-------------------------------------------
-- Chapter exercises

-- 1: No this is of kind *
data Bool' = False | True

-- 2: Yes this is of kind * -> *
data BoolAndSomethingElse a = False' a | True' a

-- 3: Yes this is of kind * -> *
data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- 4: Not `Mu` has kind (* -> *) -> *
newtype Mu f = InF { outF :: f (Mu f) }

-- 5: No `D` has kind *
-- data D =
--   D (Array Word Word) Int Int

-------------------------------------------
-- Rearrange the arguments

-- 1:
data Sum' b a =
   First' a
 | Second' b

instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap _ (Second' b) = Second' b

-- 2:
data Company a b c =
    DeepBlue a b
  | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3:
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a'') = L (f a) b (f a'')
  fmap f (R b a b'') = R b (f a) b''

-------------------------------------------
-- Write Functor instances for the following datatypes.

-- 1:
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)


-- 2:
data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a


-- 3:
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
 fmap f (Flip (K' a)) = Flip $ K' (f a)


 -- 4:
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5:
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut a) = LiftItOut (fmap g a)


-- 6:
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)


-- 7:
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)


-- 8:
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap fn (Notorious x y z) = Notorious x y (fmap fn z)


-- 9:
data List' a =
    Nil
  | Cons a (List' a)
  deriving (Eq, Show)

instance Functor List' where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)


-- 10:
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat             = NoGoat
  fmap f (OneGoat x)        = OneGoat (f x)
  fmap f (MoreGoats x y z ) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11:
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g)    = Read $ \x -> f (g x)
