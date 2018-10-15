module Exercises21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-------------------------------------------
-- Morse code revisited
-- view ch21/morse/


-------------------------------------------
-- Axing tedious code

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (traverse decodeFn a)

-- alternative
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' =
  (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn


-------------------------------------------
-- Traversable instances

-- Either
data Either' a b =
    Left' a
  | Right' b
  deriving (Show, Eq, Ord)

instance Functor (Either' a) where
  fmap _ (Left' a)  = Left' a
  fmap f (Right' a) = Right' $ f a

instance Applicative (Either' a) where
  pure           = Right'
  Left' e <*> _  = Left' e
  Right' f <*> a = fmap f a

instance Foldable (Either' a) where
  foldMap _ (Left' _)  = mempty
  foldMap f (Right' x) = f x

  foldr _ z (Left' _)  = z
  foldr f z (Right' x) = f x z

instance Traversable (Either' a) where
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' a) = Right' <$> f a



-------------------------------------------
-- Chapter Exercises:

-- 1:
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity a <*> Identity b = Identity (a b)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- 2:
newtype Constant' a b =
  Constant' { getConstant' :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
  fmap _ (Constant' a) = Constant' a

instance Monoid a =>  Applicative (Constant' a) where
  pure _ = Constant' mempty
  Constant' a <*> Constant' b = Constant' $ a <> b

instance Foldable (Constant' a) where
  foldMap _ _ = mempty

instance Traversable (Constant' a) where
  traverse _ (Constant' a) = pure (Constant' a)

instance Arbitrary a => Arbitrary (Constant' a b) where
  arbitrary = Constant' <$> arbitrary

instance Eq a => EqProp (Constant' a b) where
  (=-=) = eq


-- 3:
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Applicative Optional where
  pure          = Yep
  Nada <*> _    = Nada
  Yep f  <*> m  = fmap f m

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = optionalGen

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = do
  a <- arbitrary
  frequency [ (1, return Nada)
            , (3, return $ Yep a) ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- 4:
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) = mappend

instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure x         = Cons x Nil
  _ <*> Nil      = Nil
  Nil <*> _      = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

listGen :: Arbitrary a => Gen (List a)
listGen = do
  a0 <- arbitrary
  a1 <- arbitrary
  a2 <- arbitrary
  frequency [ (1, return Nil)
            , (2, return $ Cons a0 Nil)
            , (2, return $ Cons a0 (Cons a1 Nil))
            , (2, return $ Cons a0 (Cons a1 (Cons a2 Nil))) ]

instance Eq a => EqProp (List a) where
  (=-=) = eq


-- 5:
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- 6:
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b0 b1) = Three' a (f b0) (f b1)

instance Foldable (Three' a) where
  foldMap f (Three' _ b0 b1) = f b0 <> f b1

instance Traversable (Three' a) where
  traverse f (Three' a b c) = Three' a <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


-- 7:
data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  Pair x f <*> Pair x' y = Pair (x <> x') (f y)

instance Foldable (Pair a) where
  foldMap f (Pair _ x) = f x

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq


-- 8:
data Big a b =
  Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Monoid a => Applicative (Big a) where
  pure x = Big mempty x x
  Big a f g <*> Big b x y = Big (a <> b) (f x) (g y)

instance Foldable (Big a) where
  foldMap f (Big _ x y) = f x <> f y

instance Traversable (Big a) where
  traverse f (Big x y z) = Big x <$> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq


-- 9:
data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a x y z) = Bigger a (f x) (f y) (f z)

instance Monoid a => Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  Bigger a f _ _ <*> Bigger b x y z = Bigger (a <> b) (f x) (f y) (f z)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ x y z) = f x <> f y <> f z

instance Traversable (Bigger a) where
  traverse f (Bigger a x y z) = Bigger a <$> f x <*> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq


-- 10:
data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq


-- 11:
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node x y z) = Node (fmap f x) (f y) (fmap f z)

instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node x y z) = foldMap f x <> f y <> foldMap f z

instance Traversable Tree where
  traverse _ Empty        = pure Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Node x y z) = Node <$> traverse f x <*> f y <*> traverse f z

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof
      [ return Empty
      , Leaf <$> arbitrary
      , Node <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq


main :: IO ()
main = do
  -- Identity
  quickBatch (functor     (undefined :: Identity (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
  -- Constant
  quickBatch (functor     (undefined :: Constant' Bool (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Constant' Bool (Int, Int, [Int])))
  -- Optional
  quickBatch (functor     (undefined :: Optional (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
  -- List
  quickBatch (functor     (undefined :: List (Int, Int, [Int])))
  quickBatch (traversable (undefined :: List (Int, Int, [Int])))
