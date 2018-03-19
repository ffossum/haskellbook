{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data S n a =
  S (n a)
    a
  deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

-- instance (Applicative n, Testable (n Property), EqProp a) =>
--          EqProp (S n a) where
--   (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)
instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance (Applicative n) => Applicative (S n) where
  pure a = S (pure a) a
  (S fs f) <*> (S as a) = S (fs <*> as) (f a)

instance (Foldable n) => Foldable (S n) where
  foldr f z (S as a) = foldr f (f a z) as

instance (Monoid a, Monoid (n a)) => Monoid (S n a) where
  mempty = S mempty mempty
  mappend (S as a) (S bs b) = S (mappend as bs) (mappend a b)

instance Traversable n => Traversable (S n) where
  traverse f (S as a) = S <$> (traverse f as) <*> (f a)

type Trigger t = t (Int, Int, [Int])

testSkiFree :: IO ()
testSkiFree = do
  let trigger :: Trigger (S Maybe)
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (monoid (undefined :: S [] String))
  quickBatch (applicative trigger)
  quickBatch (traversable trigger)
