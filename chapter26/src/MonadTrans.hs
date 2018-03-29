module MonadTrans where

import           Control.Monad
import           Control.Monad.Trans.Class
import           EitherT
import           MaybeT
import           ReaderT
import           StateT

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (ReaderT r) where
  lift ma = ReaderT $ const ma

instance MonadTrans (StateT s) where
  lift ma =
    StateT $ \s -> do
      a <- ma
      return (a, s)
