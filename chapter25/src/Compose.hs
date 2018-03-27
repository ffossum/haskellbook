{-# LANGUAGE InstanceSigs #-}

module Compose where

import           Control.Applicative
import           Test.Hspec

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ fmap (fmap f) fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgh) <*> (Compose fga) = Compose $ (<*>) <$> fgh <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap fam (Compose fga) = foldMap (foldMap fam) fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse ::
       (Applicative m) => (a -> m b) -> Compose f g a -> m (Compose f g b)
  traverse h (Compose fga) = Compose <$> traverse (traverse h) fga

testCompose :: IO ()
testCompose =
  hspec $ do
    describe "Compose" $ do
      it "functor" $ do (+ 1) <$> Compose [Just 1] `shouldBe` Compose [Just 2]
      it "applicative" $ do
        pure (+ 1) <*> Compose [Just 1] `shouldBe` Compose [Just 2]
        -- [Just (+1)] <*> [Just 1]
      it "foldable" $ do foldr (+) 0 (Compose [[1, 2], [3, 4]]) `shouldBe` 10
      it "traversable" $ do
        sequenceA (Compose [Just (Just 1)]) `shouldBe` Just (Compose [Just 1])
