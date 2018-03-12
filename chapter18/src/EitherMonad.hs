module EitherMonad where

import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           MonadInstances

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance (Monoid a) => Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = (First a)
  _ <*> (First a) = (First a)
  (Second f) <*> (Second b) = Second (f b)

instance (Monoid a) => Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, first), (2, second)]
    where
      first = First <$> arbitrary
      second = Second <$> arbitrary

testEitherMonad :: IO ()
testEitherMonad = do
  quickBatch (functor (undefined :: (Sum String) SSI))
  quickBatch (applicative (undefined :: (Sum String) SSI))
  quickBatch (monad (undefined :: (Sum String) SSI))
  putStr "\nmonad matches applicative: "
  quickCheck (applicativeMatchesMonad_prop :: MatchProp (Sum String) Int String)
