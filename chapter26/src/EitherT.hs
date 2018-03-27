module EitherT where

import           Test.Hspec

newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a)
  }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ fmap (fmap f) ma

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f =
    EitherT $ do
      me <- mea
      case me of
        Left e  -> return (Left e)
        Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT (swapEither <$> ma)

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT ma) = ma >>= (either f g)

testEitherT :: IO ()
testEitherT =
  hspec $ do
    describe "EitherT" $ do
      describe "swapEitherT" $ do
        it "swaps Left to Right" $ do
          let x :: Maybe (Either String String)
              x = runEitherT (swapEitherT (EitherT (Just (Left "e"))))
          x `shouldBe` runEitherT (EitherT (Just (Right "e")))
        it "swaps Right to Left" $ do
          let x :: Maybe (Either String String)
              x = runEitherT (swapEitherT (EitherT (Just (Right "a"))))
          x `shouldBe` runEitherT (EitherT (Just (Left "a")))
      describe "eitherT" $ do
        it "gets Right value" $ do
          let x :: EitherT Int Maybe Int
              x = EitherT (Just (Right 1))
          eitherT pure (pure . (+ 1)) x `shouldBe` Just 2
        it "gets Left value" $ do
          let x :: EitherT Int Maybe Int
              x = EitherT (Just (Left 1))
          eitherT (pure . (subtract 1)) pure x `shouldBe` Just 0
      it "has monad instance" $ do
        let x :: EitherT Int Maybe Int
            x = do
              i <- EitherT (Just (Right 1))
              return (i + 1)
        runEitherT x `shouldBe` (Just (Right 2))
