module EitherFunctor where

import           FunctorInstances
import           Test.Hspec
import           Test.QuickCheck

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, first), (3, second)]
    where
      first = First <$> arbitrary
      second = Second <$> arbitrary

testEither :: IO ()
testEither =
  hspec $ do
    describe "Functor instances" $ do
      describe "Sum (Either)" $ do
        it "identity" $ property $ (functorIdentity :: IdentityTest (Sum Int))
        it "compose" $ property $ (functorCompose' :: ComposeTest (Sum Int))
