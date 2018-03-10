module Possibly where

import           FunctorInstances
import           Test.Hspec
import           Test.QuickCheck

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap _ LolNope     = LolNope

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (3, Yeppers <$> arbitrary)]

testPossibly :: IO ()
testPossibly =
  hspec $ do
    describe "Functor instances" $ do
      describe "Possibly (Maybe)" $ do
        it "identity" $ property $ (functorIdentity :: IdentityTest Possibly)
        it "compose" $ property $ (functorCompose' :: ComposeTest Possibly)
