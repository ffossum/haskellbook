module MonadInstances where

import           Control.Monad
import           Prelude                  hiding (Left, Right)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- [1]
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

-- [2]
data PhhhbbtttEither b a
  = Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left a)  = Left (f a)
  fmap _ (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
  pure a = Left a
  (Left f) <*> (Left a) = Left (f a)
  (Right b) <*> _ = (Right b)
  _ <*> (Right b) = (Right b)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Left a) >>= f = f a
  (Right b) >>= _ = (Right b)

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = frequency [(2, Left <$> arbitrary), (1, Right <$> arbitrary)]

-- [3]
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- [4]
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
  mappend (Cons a as) bs = Cons a (mappend as bs)
  mappend Nil bs         = bs

instance Applicative List where
  pure a = Cons a Nil
  (Cons f fs) <*> as = (fmap f as) `mappend` (fs <*> as)
  _ <*> Nil = Nil
  Nil <*> _ = Nil

instance Monad List where
  return = pure
  (Cons a as) >>= f = (f a) `mappend` (as >>= f)
  Nil >>= _ = Nil

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

type SSI = (String, String, Int)

applicativeMatchesMonad_prop :: (Monad m, Eq (m b)) => MatchProp m a b
applicativeMatchesMonad_prop f m = ((pure f) <*> m) == ((pure f) `ap` m)

type MatchProp m a b = (a -> b) -> (m a) -> Bool

testMonadInstances :: IO ()
testMonadInstances = do
  putStrLn "Nope:"
  quickBatch (functor (undefined :: Nope SSI))
  quickBatch (applicative (undefined :: Nope SSI))
  quickBatch (monad (undefined :: Nope SSI))
  putStr "\nmonad matches applicative: "
  quickCheck (applicativeMatchesMonad_prop :: MatchProp Nope Int String)
  putStrLn "PhhhbbtttEither:"
  quickBatch (functor (undefined :: (PhhhbbtttEither String) SSI))
  quickBatch (applicative (undefined :: (PhhhbbtttEither String) SSI))
  quickBatch (monad (undefined :: (PhhhbbtttEither String) SSI))
  putStr "\nmonad matches applicative: "
  quickCheck
    (applicativeMatchesMonad_prop :: MatchProp (PhhhbbtttEither String) Int String)
  putStrLn "Identity:"
  quickBatch (functor (undefined :: Identity SSI))
  quickBatch (applicative (undefined :: Identity SSI))
  quickBatch (monad (undefined :: Identity SSI))
  putStr "\nmonad matches applicative: "
  quickCheck (applicativeMatchesMonad_prop :: MatchProp Identity Int String)
  putStrLn "List:"
  quickBatch (monoid (undefined :: List SSI))
  quickBatch (functor (undefined :: List SSI))
  quickBatch (applicative (undefined :: List SSI))
  quickBatch (monad (undefined :: List SSI))
  putStr "\nmonad matches applicative: "
  quickCheck (applicativeMatchesMonad_prop :: MatchProp List Int String)

j :: Monad m => m (m a) -> m a
j = (>>= id)

testJ :: IO ()
testJ =
  hspec $
  describe "j" $ do
    it "joins lists" $ (j [[1, 2], [], [3]]) `shouldBe` [1, 2, 3 :: Int]
    it "joins maybes" $ (j (Just (Just 1))) `shouldBe` Just (1 :: Int)
    it "joins Just Nothing" $
      (j (Just Nothing)) `shouldBe` (Nothing :: Maybe Int)
    it "joins nothing" $ (j Nothing) `shouldBe` (Nothing :: Maybe Int)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
  a' <- ma
  b <- mb
  return (f a' b)

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do
  a' <- ma
  f <- mf
  return (f a')

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a':as) f = do
  b <- (f a')
  bs <- meh as f
  return (b : bs)

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id

testFlipType :: IO ()
testFlipType =
  hspec $
  describe "flipType" $ do
    it "flips list of Maybes" $
      (flipType [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3 :: Int])
