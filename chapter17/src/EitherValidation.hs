module EitherValidation where

import           ListApplicative

import           Test.QuickCheck          (Arbitrary, arbitrary, frequency)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Success f) <*> (Success a) = Success (f a)
  (Success _) <*> (Failure e) = Failure e
  (Failure e) <*> (Success _) = Failure e
  (Failure e1) <*> (Failure e2) = Failure (mappend e1 e2)

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, e), (2, a)]
    where
      e = Failure <$> arbitrary
      a = Success <$> arbitrary

testValidation :: IO ()
testValidation = do
  putStrLn "Validation:"
  quickBatch (functor (undefined :: (Validation String) SSI))
  quickBatch (applicative (undefined :: (Validation String) SSI))
