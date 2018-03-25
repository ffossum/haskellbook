module Data.PositiveInteger where

import           Control.Applicative
import           MaybeSuccess
import           Test.Hspec
import           Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0' .. '9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

testPositiveInteger :: IO ()
testPositiveInteger =
  hspec $ do
    describe "PositiveInteger" $ do
      it "parseDigit" $ do
        maybeSuccess (parseString parseDigit mempty "123") `shouldBe` (Just '1')
        maybeSuccess (parseString parseDigit mempty "abc") `shouldBe` Nothing
      it "base10Integer" $ do
        maybeSuccess (parseString base10Integer mempty "123abc") `shouldBe`
          (Just 123)
        maybeSuccess (parseString base10Integer mempty "abc") `shouldBe` Nothing
      it "base10Integer'" $ do
        maybeSuccess (parseString base10Integer' mempty "-123abc") `shouldBe`
          Just (-123)
        maybeSuccess (parseString base10Integer' mempty "+123") `shouldBe`
          Just 123
        maybeSuccess (parseString base10Integer' mempty "123") `shouldBe`
          Just 123

base10Integer' :: Parser Integer
base10Integer' =
  (try $ (char '-') >> (negate <$> base10Integer)) <|>
  ((optional (char '+')) >> base10Integer)
