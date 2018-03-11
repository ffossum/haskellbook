module ListApplicative where

import           Control.Applicative
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil a          = a
  mappend a Nil          = a
  mappend (Cons a as) bs = Cons a (mappend as bs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> as = (f <$> as) `mappend` (fs <*> as)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [nil, cons]
    where
      nil = return Nil
      cons = do
        a <- arbitrary
        as <- arbitrary
        return (Cons a as)

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons a as)
  | n <= 0 = Nil
  | otherwise = Cons a (take' (n - 1) as)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
        in take' 100 l
      ys' =
        let (ZipList' l) = ys
        in take' 100 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a rest)
    where
      (ZipList' rest) = pure a
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons a as)) = ZipList' (Cons (f a) rest)
    where
      (ZipList' rest) = (ZipList' fs) <*> (ZipList' as)

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

type SSI = (String, String, Int)

main :: IO ()
main = do
  putStrLn "List:"
  quickBatch (monoid (undefined :: List String))
  quickBatch (functor (undefined :: List SSI))
  quickBatch (applicative (undefined :: List SSI))
  putStrLn "ZipList:"
  quickBatch (monoid (undefined :: ZipList' String))
  quickBatch (functor (undefined :: ZipList' SSI))
  quickBatch (applicative (undefined :: ZipList' SSI))
