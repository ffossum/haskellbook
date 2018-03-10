module FunctorInstances where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return (Pair a1 a2)

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return (Three' a b1 b2)

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return (Four' a1 a2 a3 b)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> (a -> b) -> (b -> c) -> Bool
functorCompose x f g = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IdentityTest f = f Int -> Bool

type ComposeTest f = f Int -> Fun Int String -> Fun String Int -> Bool

testFunctors :: IO ()
testFunctors =
  hspec $ do
    describe "Functor instances" $ do
      describe "Identity" $ do
        it "identity" $ property $ (functorIdentity :: IdentityTest Identity)
        it "compose" $ property $ (functorCompose' :: ComposeTest Identity)
      describe "Pair" $ do
        it "identity" $ property $ (functorIdentity :: IdentityTest Pair)
        it "compose" $ property $ (functorCompose' :: ComposeTest Pair)
      describe "Two" $ do
        it "identity" $ property $ (functorIdentity :: IdentityTest (Two Int))
        it "compose" $ property $ (functorCompose' :: ComposeTest (Two Int))
      describe "Three" $ do
        it "identity" $
          property $ (functorIdentity :: IdentityTest (Three Int Int))
        it "compose" $
          property $ (functorCompose' :: ComposeTest (Three Int Int))
      describe "Three'" $ do
        it "identity" $
          property $ (functorIdentity :: IdentityTest (Three' Int))
        it "compose" $ property $ (functorCompose' :: ComposeTest (Three' Int))
      describe "Four" $ do
        it "identity" $
          property $ (functorIdentity :: IdentityTest (Four Int Int Int))
        it "compose" $
          property $ (functorCompose' :: ComposeTest (Four Int Int Int))
      describe "Four'" $ do
        it "identity" $ property $ (functorIdentity :: IdentityTest (Four' Int))
        it "compose" $ property $ (functorCompose' :: ComposeTest (Four' Int))
