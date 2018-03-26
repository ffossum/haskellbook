module Data.IPv4 where

import           Data.Word
import           MaybeSuccess
import           Test.Hspec
import           Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

parseWord32 :: Parser Word32
parseWord32 = do
  d <- decimal
  if (d >= 0 && d <= 255)
    then pure (fromIntegral d)
    else fail "value too large, must be 0-255"

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  oct1 <- parseWord32
  char '.'
  oct2 <- parseWord32
  char '.'
  oct3 <- parseWord32
  char '.'
  oct4 <- parseWord32
  let value :: Word32
      value = (oct1 * 256 ^ 3) + (oct2 * 256 ^ 2) + oct3 * 256 + oct4
  return $ IPAddress value

testIP :: IO ()
testIP =
  hspec $ do
    describe "IPv4" $ do
      it "parses IPv4 correctly" $ do
        maybeSuccess (parseString parseIPAddress mempty "172.16.254.1") `shouldBe`
          Just (IPAddress 2886794753)
        maybeSuccess (parseString parseIPAddress mempty "204.120.0.15") `shouldBe`
          Just (IPAddress 3430416399)
