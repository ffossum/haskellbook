module Data.PhoneNumber where

import           Control.Applicative
import           MaybeSuccess
import           Test.Hspec
import           Text.Trifecta

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange
              LineNumber
  deriving (Eq, Show)

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = do
  try (parens $ parseDigits 3) <|> parseDigits 3

parseDigits :: Int -> Parser Int
parseDigits n = read <$> count n digit

parsePhone :: Parser PhoneNumber
parsePhone = do
  skipOptional (try (digit >> char '-'))
  npa <- token $ parseNumberingPlanArea
  skipOptional (char '-')
  ex <- token $ parseDigits 3
  skipOptional (char '-')
  ln <- parseDigits 4
  return (PhoneNumber npa ex ln)

testPhoneNumber :: IO ()
testPhoneNumber =
  hspec $ do
    describe "PhoneNumber" $ do
      describe "parsePhone" $ do
        it "example 1" $ do
          maybeSuccess (parseString parsePhone mempty "123-456-7890") `shouldBe`
            Just (PhoneNumber 123 456 7890)
        it "example 2" $ do
          maybeSuccess (parseString parsePhone mempty "1234567890") `shouldBe`
            Just (PhoneNumber 123 456 7890)
        it "example 3" $ do
          maybeSuccess (parseString parsePhone mempty "(123) 456-7890") `shouldBe`
            Just (PhoneNumber 123 456 7890)
        it "example 4" $ do
          maybeSuccess (parseString parsePhone mempty "1-123-456-7890") `shouldBe`
            Just (PhoneNumber 123 456 7890)
