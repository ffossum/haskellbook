module LibraryFunctions where

import           Data.Foldable hiding (elem, fold, foldMap, length, maximum,
                                minimum, null, product, sum, toList)
import           Data.Maybe
import           Data.Monoid
import           Prelude       hiding (elem, foldMap, length, maximum, minimum,
                                null, product, sum)
import           Test.Hspec

newtype Max a = Max
  { getMax :: Maybe a
  } deriving (Eq, Ord, Show)

instance (Ord a) => Monoid (Max a) where
  mempty = Max Nothing
  mappend (Max Nothing) a   = a
  mappend a (Max Nothing)   = a
  mappend (Max a1) (Max a2) = Max (max a1 a2)

newtype Min a = Min
  { getMin :: Maybe a
  } deriving (Eq, Ord, Show)

instance (Ord a) => Monoid (Min a) where
  mempty = Min Nothing
  mappend (Min Nothing) a   = a
  mappend a (Min Nothing)   = a
  mappend (Min a1) (Min a2) = Min (min a1 a2)

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum -- foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product -- foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr f False
  where
    f a b = (a == x) || b

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs
  | null xs = Nothing
  | otherwise = getMin $ foldMap (Min . Just) xs

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs
  | null xs = Nothing
  | otherwise = getMax $ foldMap (Max . Just) xs

null :: (Foldable t) => t a -> Bool
null = (== 0) . length

length :: (Foldable t) => t a -> Int
length = foldr f 0
  where
    f _ b = b + 1

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap toMonoid = foldr f mempty
  where
    f a b = (toMonoid a) <> b

testLibrary :: IO ()
testLibrary =
  hspec $ do
    describe "sum" $ do
      it "adds all numbers in a list" $ do
        sum [1, 2, 3, 4, 5] `shouldBe` (15 :: Int)
    describe "product" $ do
      it "multiplies all numbers in a list" $ do
        product [1, 2, 3, 4, 5] `shouldBe` (120 :: Int)
    describe "elem" $ do
      it "true if element in is list" $ do
        elem 3 [1, 2, 3, 4, 5 :: Int] `shouldBe` True
      it "false if element is not in list" $ do
        elem 6 [1, 2, 3, 4, 5 :: Int] `shouldBe` False
      it "does not evaluate elements after match is found" $ do
        elem 3 [1, 2, 3 :: Int, undefined] `shouldBe` True
    describe "minimum" $ do
      it "finds smallest number in list" $ do
        minimum [1, 2, 3, 0, 5 :: Int] `shouldBe` Just 0
      it "nothing for empty list" $ do
        minimum [] `shouldBe` (Nothing :: Maybe Int)
    describe "maximum" $ do
      it "finds largest number in list" $ do
        maximum [1, 2, 3, 0, 5 :: Int] `shouldBe` Just 5
      it "nothing for empty list" $ do
        maximum [] `shouldBe` (Nothing :: Maybe Int)
    describe "null" $ do
      it "returns whether list is empty" $ do
        null [] `shouldBe` True
        null [1, 2, 3 :: Int] `shouldBe` False
    describe "length" $ do
      it "returns length of list" $ do
        length [] `shouldBe` 0
        length [1, 2, 3 :: Int] `shouldBe` 3
    describe "toList" $ do
      it "turns Maybe into List" $ do
        toList (Just 1) `shouldBe` ([1] :: [Int])
        toList Nothing `shouldBe` ([] :: [Int])
    describe "fold" $ do
      it "sums list of Sums" $ do
        fold [Sum 1, Sum 2, Sum 3] `shouldBe` (Sum 6 :: Sum Int)
