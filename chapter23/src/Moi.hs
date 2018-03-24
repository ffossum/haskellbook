{-# LANGUAGE InstanceSigs #-}

module Moi where

import           Test.Hspec

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi
      (\s ->
         let (a, nextS) = g s
         in (f a, nextS))

testMoi :: IO ()
testMoi =
  hspec $ do
    describe "Moi functor" $ do
      it "can add one" $ do
        (runMoi ((+ 1) <$> (Moi $ \s -> (0, s))) 0) `shouldBe`
          ((1, 0) :: (Int, Int))

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi
      (\s ->
         let (f', s2) = f s
             (a, s3) = g s2
         in (f' a, s3))

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi
      (\s ->
         let (a, s2) = f s
             (Moi g') = g a
         in g' s2)
