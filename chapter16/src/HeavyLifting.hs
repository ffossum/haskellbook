module HeavyLifting where

import           Test.Hspec

-- [1]
a = fmap (+ 1) $ read "[1]" :: [Int]

-- [2]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- [3]
c = fmap (* 2) (\x -> x - 2)

-- [4]
d = fmap ((return '1' ++) . show) (\x -> [x,1 .. 3])

-- [5]
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = read <$> ("123" ++) <$> show <$> ioi
  in (* 3) <$> changed

runTests :: IO ()
runTests =
  hspec $ do
    describe "heavy lifting" $ do
      it "a" $ a `shouldBe` [2]
      it "b" $ b `shouldBe` Just ["Hi,lol", "Hellolol"]
      it "c" $ c 1 `shouldBe` (-2)
      it "d" $ d 0 `shouldBe` "1[0,1,2,3]"
      it "e" $ e >>= (`shouldBe` 3693)
