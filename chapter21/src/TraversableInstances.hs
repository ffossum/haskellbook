module TraversableInstances where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

type Trigger t = t (Int, Int, [Int])

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> (f a)

testIdentity :: IO ()
testIdentity = do
  let trigger :: Trigger Identity
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Show, Ord)

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a1) <*> (Constant a2) = Constant (a1 `mappend` a2)

instance Foldable (Constant a) where
  foldr _ z (Constant _) = z
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

testConstant :: IO ()
testConstant = do
  let trigger :: Trigger (Constant String)
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (3, Yep <$> arbitrary)]

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  _ <*> Nada = Nada
  Yep f <*> Yep a = Yep (f a)

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> (f a)

testOptional :: IO ()
testOptional = do
  let trigger :: Trigger Optional
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Ord, Show)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [(1, nil), (2, cons)]
    where
      nil = return Nil
      cons = do
        a <- arbitrary
        as <- arbitrary
        return (Cons a as)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil bs         = bs
  mappend (Cons a as) bs = Cons a (as `mappend` bs)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> as = (f <$> as) `mappend` (fs <*> as)

instance Foldable List where
  foldr _ z Nil         = z
  foldr f z (Cons a as) = f a (foldr f z as)

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons a as) = mappend <$> (pure <$> (f a)) <*> (traverse f as)
  -- sequenceA Nil = pure Nil
  -- sequenceA (Cons fa fas) = mappend <$> (pure <$> fa) <*> (sequenceA fas)

testList :: IO ()
testList = do
  let trigger :: Trigger List
      trigger = undefined
  quickBatch (monoid (undefined :: List Int))
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Ord, Show)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (Three a1 b1 f) <*> (Three a2 b2 c) =
    Three (mappend a1 a2) (mappend b1 b2) (f c)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> (f c)

testThree :: IO ()
testThree = do
  let trigger :: Trigger (Three String String)
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)

data Pair a b =
  Pair a
       b
  deriving (Eq, Ord, Show)

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance (Monoid a) => Applicative (Pair a) where
  pure b = Pair mempty b
  (Pair a1 f) <*> (Pair a2 b) = Pair (mappend a1 a2) (f b)

instance Foldable (Pair a) where
  foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = (Pair a) <$> (f b)

testPair :: IO ()
testPair = do
  let trigger :: Trigger (Pair String)
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)

data Big a b =
  Big a
      b
      b
  deriving (Eq, Ord, Show)

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance (Monoid a) => Applicative (Big a) where
  pure b = Big mempty b b
  (Big a1 f1 f2) <*> (Big a2 b1 b2) = Big (mappend a1 a2) (f1 b1) (f2 b2)

instance Foldable (Big a) where
  foldr f z (Big _ b1 b2) = f b1 (f b2 z)

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = (Big a) <$> (f b1) <*> (f b2)

testBig :: IO ()
testBig = do
  let trigger :: Trigger (Big String)
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)

data Bigger a b =
  Bigger a
         b
         b
         b
  deriving (Eq, Ord, Show)

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance (Monoid a) => Applicative (Bigger a) where
  pure b = Bigger mempty b b b
  (Bigger a1 f1 f2 f3) <*> (Bigger a2 b1 b2 b3) =
    Bigger (mappend a1 a2) (f1 b1) (f2 b2) (f3 b3)

instance Foldable (Bigger a) where
  foldr f z (Bigger _ b1 b2 b3) = foldr f z [b1, b2, b3]

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = (Bigger a) <$> (f b1) <*> (f b2) <*> (f b3)

testBigger :: IO ()
testBigger = do
  let trigger :: Trigger (Bigger String)
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)

main :: IO ()
main = do
  testIdentity
  testConstant
  testOptional
  testList
  testThree
  testPair
  testBig
  testBigger
