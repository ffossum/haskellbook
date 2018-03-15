module FoldableInstances where

import           Data.Monoid
import           Test.Hspec

-- [1]
data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z

-- [2]
data Two a b =
  Two a
      b

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

-- [3]
data Three a b c =
  Three a
        b
        c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

-- [4]
data Three' a b =
  Three' a
         b
         b

instance Foldable (Three' a) where
  foldr f z (Three' _ b1 b2) = f b1 (f b2 z)

-- [5]
data Four' a b =
  Four' a
        b
        b
        b

instance Foldable (Four' a) where
  foldr f z (Four' _ b1 b2 b3) = foldr f z [b1, b2, b3]

-- Thinking cap time
filterF ::
     (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF predicate xs = foldMap f xs
  where
    f x =
      if predicate x
        then pure x
        else mempty

testInstances :: IO ()
testInstances =
  hspec $ do
    describe "Foldable instances" $ do
      describe "Constant" $ do
        it "foldr" $ do foldr (+) 0 (Constant 1) `shouldBe` (1 :: Int)
      describe "Two" $ do
        it "foldr" $ do foldr (+) 0 (Two "Hi" 1) `shouldBe` (1 :: Int)
      describe "Three'" $ do
        it "does not evaluate more elements than necessary" $ do
          foldr (||) False (Three' "Hi" True undefined) `shouldBe` True
      describe "filterF" $ do
        it "filters list" $ do filterF odd [1, 2, 3 :: Int] `shouldBe` [1, 3]
