{-# LANGUAGE InstanceSigs #-}

module StateT where

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) =
    StateT $
    (\s ->
       let ma = sma s
       in fmap f' ma)
    where
      f' (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateT smf) (StateT sma) =
    StateT $ \s -> do
      (f, s2) <- smf s
      (a, s3) <- sma s2
      return (f a, s3)

instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f =
    StateT $ \s -> do
      (a, s2) <- sma s
      runStateT (f a) s2
