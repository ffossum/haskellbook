module Identity where

newtype Identity a = Identity
  { runIdentity :: a
  } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

newtype IdentityT f a = IdentityT
  { runIdentityT :: f a
  } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)
  (IdentityT mf) <*> (IdentityT ma) = IdentityT $ mf <*> ma

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= (runIdentityT . f)
