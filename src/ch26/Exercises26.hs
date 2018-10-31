{-# LANGUAGE FlexibleContexts #-}

module Exercises26 where

import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (Reader)


-------------------------------------------
-- MaybeT

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m
      => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)


-------------------------------------------
-- EitherT

newtype EitherT e m a =
  EitherT {runEitherT :: m (Either e a)}

-- 1:
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

-- 2:
instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

-- 3:
instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f =
    EitherT $ do
      v <- mea
      case v of
        Left x -> return $ Left x
        Right y -> runEitherT (f y)

-- 4:
swapEither :: Either a b -> Either b a
swapEither eab = case eab of
  Left x -> Right x
  Right x -> Left x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

-- 5:
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mab) = either f g =<< mab



-------------------------------------------
-- ReaderT

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))

  ReaderT fmab <*> ReaderT rma =
    ReaderT $ (<*>) <$> fmab <*> rma


instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r


-------------------------------------------
-- StateT
newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

-- 1:
instance Functor m => Functor (StateT s m) where
  fmap f (StateT smaS) =
    StateT $ (fmap . fmap) tup smaS
      where tup (a,b) = (f a, b)

-- 2:
instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  -- (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT f <*> StateT sma =
    StateT $ \s -> do
      (g, s') <- f s
      (x, s'') <- sma s'
      return (g x, s'')

-- 3:
instance Monad m => Monad (StateT s m) where
  return = pure
  StateT sma >>= f =
    StateT $ \s -> do
      (v, s') <- sma s
      runStateT (f v) s'



-------------------------------------------
-- STACK‘EMUP
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: ()-> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- Exercise: Wrap It Up
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT $ ExceptT $ ReaderT $ fmap pure (const (Right (Just 1)))


-------------------------------------------
-- Exercises: Lift More
-- 1:
instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

-- 2:
instance MonadTrans (StateT s) where
  lift ma =
    StateT $ \s -> do
      a <- ma
      return (a, s)


-------------------------------------------
-- Example MonadIO instances
-- this is already inported
-- instance MonadIO m => MonadIO (IdentityT m) where
--   liftIO = IdentityT . liftIO

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

-- 1:
instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- 2:
instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

-- 3:
-- already declared further up!
-- instance MonadTrans (StateT r) where
--   lift ma =
--     StateT $ \s -> do
--       a <- ma
--       return (s, a)

instance MonadIO m => MonadIO (StateT r m) where
  liftIO = lift . liftIO


-------------------------------------------
-- Chapter Exercises:

-- view /ChapterExercises.hs

-- view /HitCouterExercise.hs

-- TODO: Morra Game
