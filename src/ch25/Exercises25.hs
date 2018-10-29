{-# LANGUAGE InstanceSigs #-}

module Exercises25 where

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g)
       => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f
      => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h)
       => Functor (Three f g h) where
  fmap f (Three fgha) =
    Three $ (fmap . fmap . fmap) f fgha


-------------------------------------------
-- GOTCHA! Exercise time

instance (Applicative f, Applicative g)
       => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ ((<*>) $ fmap (<*>) f) a


-------------------------------------------
-- Exercises: Compose Instances

-- 1:
instance (Foldable f, Foldable g)
       => Foldable (Compose f g) where
  foldMap h (Compose fga) = foldMap (foldMap h) fga

-- 2:
instance (Traversable f, Traversable g)
       => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga


-------------------------------------------
-- And now for something completely different

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1:
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)
  first f (Deux x y)   = Deux (f x) y
  second f (Deux x y)  = Deux x (f y)


-- 2:
data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const x) = Const (f x)
  first f (Const x)   = Const (f x)
  second _ (Const x)  = Const x

-- 3:
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
  first f (Drei a b c)   = Drei a (f b) c
  second f (Drei a b c)  = Drei a b (f c)

-- 4:
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)
  first f (SuperDrei a b)   = SuperDrei a (f b)
  second _ (SuperDrei a b)  = SuperDrei a b

-- 5:
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a
  first _ (SemiDrei a)   = SemiDrei a
  second _ (SemiDrei a)  = SemiDrei a

-- 6:
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)
  first f (Quadzzz a b c d)   = Quadzzz a b (f c) d
  second f (Quadzzz a b c d)  = Quadzzz a b c (f d)

-- 7:
data Either' a b
  = Left' a
  | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' a)  = Left' (f a)
  bimap _ g (Right' a) = Right' (g a)

  first f (Left' a)    = Left' (f a)
  first _ (Right' a)   = Right' a

  second _ (Left' a)   = Left' a
  second f (Right' a)  = Right' (f a)


-------------------------------------------
-- IdentityT

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Show, Eq)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative m =>  Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  IdentityT fab <*> IdentityT fa = IdentityT (fab <*> fa)

instance Monad m => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
