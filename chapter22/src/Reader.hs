{-# LANGUAGE InstanceSigs #-}

module Reader where

import           Test.Hspec
import           Test.QuickCheck

newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance (CoArbitrary r, Arbitrary a) => Arbitrary (Reader r a) where
  arbitrary = Reader <$> arbitrary

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) = Reader (\r -> rab r (ra r))

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) aRb =
    Reader
      (\r ->
         let a = ra r
             (Reader rb) = aRb a
             b = rb r
         in b)

type FunctorProp r a b = Fun r a -> Fun a b -> r -> Bool

reader_fmap_prop :: (Eq b) => FunctorProp r a b
reader_fmap_prop (Fn ra) (Fn ab) r = readerResult == funResult
  where
    readerResult = runReader (ab <$> Reader ra) $ r
    funResult = (ab <$> ra) r

type ApplicativeProp r a b = Fun (r, a) b -> Fun r a -> r -> Bool

reader_apply_prop :: (Eq b) => ApplicativeProp r a b
reader_apply_prop (Fn2 rab) (Fn ab) r = readerResult == funResult
  where
    readerResult = runReader (Reader rab <*> Reader ab) $ r
    funResult = (rab <*> ab) r

reader_pure_prop :: (Eq a) => r -> a -> Bool
reader_pure_prop r a = readerResult == funResult
  where
    readerResult = runReader (pure a) $ r
    funResult = (pure a) r

main :: IO ()
main = do
  hspec $ do
    describe "Reader typeclass instances" $ do
      it "fmap behaves like function fmap" $ do
        property (reader_fmap_prop :: FunctorProp Int Int Int)
      it "<*> behaves like function <*>" $ do
        property (reader_apply_prop :: ApplicativeProp Int Int Int)
      it "pure behaves like function pure" $ do
        property (reader_pure_prop :: Int -> Int -> Bool)
