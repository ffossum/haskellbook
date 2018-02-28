import           Addition
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main =
  hspec $ do
    describe "Addition" $ do
      it "15 divided by 3 is 5" $ do dividedBy 15 3 `shouldBe` (5, 0)
      it "22 divided by 5 is 4 remainder 2" $ do
        dividedBy 22 5 `shouldBe` (4, 2)
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)
    describe "Multiplication" $ do
      it "5 times 5 is 25" $ do mult 5 5 `shouldBe` 25
      it "0 times 10 is 0" $ do mult 0 10 `shouldBe` 0
      it "3 times (-4) is (-12)" $ do mult 3 (-4) `shouldBe` (-12)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
