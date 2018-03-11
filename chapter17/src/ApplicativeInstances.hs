module ApplicativeInstances where

import           ListApplicative

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- [1]
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f1 f2) <*> (Pair a1 a2) = Pair (f1 a1) (f2 a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return (Pair a1 a2)

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

-- [2]
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure b = Two mempty b
  (Two a1 f) <*> (Two a2 b) = Two (mappend a1 a2) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- [3]
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (Three a1 b1 f) <*> (Three a2 b2 c) =
    Three (mappend a1 a2) (mappend b1 b2) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- [4]
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where
  pure b = (Three' mempty b b)
  (Three' a1 f1 f2) <*> (Three' a2 b1 b2) =
    Three' (mappend a1 a2) (f1 b1) (f2 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return (Three' a b1 b2)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- [5]
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (Four a1 b1 c1 f) <*> (Four a2 b2 c2 d) =
    Four (mappend a1 a2) (mappend b1 b2) (mappend c1 c2) (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- [6]
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (Four' a1 a2 a3 f) <*> (Four' a4 a5 a6 b) =
    Four' (mappend a1 a4) (mappend a2 a5) (mappend a3 a6) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return (Four' a1 a2 a3 b)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

testInstances :: IO ()
testInstances = do
  putStr "List:"
  quickBatch (functor (undefined :: Pair SSI))
  quickBatch (applicative (undefined :: Pair SSI))
  putStr "Two:"
  quickBatch (functor (undefined :: (Two String) SSI))
  quickBatch (applicative (undefined :: (Two String) SSI))
  putStr "Three:"
  quickBatch (functor (undefined :: (Three String String) SSI))
  quickBatch (applicative (undefined :: (Three String String) SSI))
  putStr "Three':"
  quickBatch (functor (undefined :: (Three' String) SSI))
  quickBatch (applicative (undefined :: (Three' String) SSI))
  putStr "Four:"
  quickBatch (functor (undefined :: (Four String String String) SSI))
  quickBatch (applicative (undefined :: (Four String String String) SSI))
  putStr "Four':"
  quickBatch (functor (undefined :: (Four' String) SSI))
  quickBatch (applicative (undefined :: (Four' String) SSI))
