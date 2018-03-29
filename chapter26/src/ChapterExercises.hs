module ChapterExercises where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Test.Hspec

rDec :: Num a => Reader a a
rDec = reader $ (subtract 1)

rShow :: Show a => Reader a String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT $ \r -> do
    liftIO $ putStr "Hi: " >> print r
    return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum =
  StateT $ \s -> do
    liftIO $ putStr "Hi: " >> print s
    return (show s, s + 1)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO $ getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)

testExercises :: IO ()
testExercises =
  hspec $ do
    describe "Chapter exercises" $ do
      it "rDec" $ do
        runReader rDec 1 `shouldBe` 0
        fmap (runReader rDec) [1 .. 10] `shouldBe` [0 .. 9]
      it "rShow" $ do
        runReader rShow 1 `shouldBe` "1"
        fmap (runReader rShow) [1 .. 10] `shouldBe`
          ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
      it "rPrintAndInc" $ do
        x <- traverse (runReaderT rPrintAndInc) [1 .. 10]
        x `shouldBe` [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
      it "sPrintIncAccum" $ do
        x <- mapM (runStateT sPrintIncAccum) [1 .. 5]
        x `shouldBe` [("1", 2), ("2", 3), ("3", 4), ("4", 5), ("5", 6)]
