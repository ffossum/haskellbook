module ReaderPractice where

import           Control.Applicative
import           Data.Maybe

import           Test.Hspec

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (a, a)
  where
    a = z' n

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (> 3) <*> (< 8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(> 3), (< 8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

testPractice :: IO ()
testPractice = do
  hspec $ do
    describe "warm up" $ do
      it "x1" $ do x1 `shouldBe` (Just (6, 9))
      it "x2" $ do x2 `shouldBe` Nothing
      it "x3" $ do (x3 3) `shouldBe` (Just 9, Just 9)
      it "summed" $ do
        (summed <$> ((,) <$> xs <*> ys)) `shouldBe` (Just 15)
        (fmap summed ((,) <$> xs <*> zs)) `shouldBe` Nothing
      it "bolt" $ do
        bolt 7 `shouldBe` True
        fmap bolt z `shouldBe` [True, False, False]

main :: IO ()
main = do
  print $ foldr (&&) True (sequA (1 :: Integer))
  print $ sequA (fromMaybe 0 s')
  print $ bolt (fromMaybe 0 ys)
