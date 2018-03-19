module Tree where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

type Trigger t = t (Int, Int, [Int])

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = frequency [(1, empty), (2, leaf), (2, node)]
    where
      empty = return Empty
      leaf = Leaf <$> arbitrary
      node = do
        l <- arbitrary
        a <- arbitrary
        r <- arbitrary
        return (Node l a r)

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap _ Empty          = Empty
  fmap f (Leaf a)       = Leaf (f a)
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
  foldr _ z Empty        = z
  foldr f z (Leaf a)     = f a z
  foldr f z (Node l a r) = foldr f (f a (foldr f z r)) l
  -- foldMap _ Empty = mempty
  -- foldMap m (Leaf a) = m a
  -- foldMap m (Node l a r) = (foldMap m l) `mappend` (m a) `mappend` (foldMap m r)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node l a r) = Node <$> (traverse f l) <*> (f a) <*> (traverse f r)

testTree :: IO ()
testTree = do
  let trigger :: Trigger Tree
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
