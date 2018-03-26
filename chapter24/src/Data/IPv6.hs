module Data.IPv6 where

import           Control.Applicative
import           Data.IPv4
import           Data.Word
import           MaybeSuccess
import           Numeric
import           Test.Hspec
import           Text.Trifecta

data IPAddress6 =
  IPAddress6 Word64
             Word64
  deriving (Eq, Ord)

instance Show IPAddress6 where
  show (IPAddress6 w1 w2) =
    show $ (16 ^ 16) * (fromIntegral w1) + (fromIntegral w2)

data Group
  = Group Word64
  | Collapse
  deriving (Eq, Ord)

parseGroup :: Parser Group
parseGroup = do
  hex <- many hexDigit
  if (length hex > 4)
    then fail "value too large"
    else if (hex == "")
           then return Collapse
           else case (readHex hex) of
                  [(value, _)] -> return (Group value)
                  _            -> fail "invalid hex"

parseGroups :: Parser [Group]
parseGroups = do
  firstGroup <- parseGroup
  otherGroups <- many $ char ':' >> parseGroup
  fullList <- pure (firstGroup : otherGroups)
  let collapses = length $ filter (== Collapse) fullList
  if collapses > 1
    then fail "max 1 collapse allowed"
    else pure fullList

uncollapse :: [Group] -> [Word64]
uncollapse gs =
  let missingL = 8 - length gs
  in do g <- gs
        case g of
          (Group value) -> [value]
          Collapse      -> take (missingL + 1) (repeat 0)

wordsToIp :: [Word64] -> Maybe IPAddress6
wordsToIp [w1, w2, w3, w4, w5, w6, w7, w8] =
  Just $ IPAddress6 (add4 w1 w2 w3 w4) (add4 w5 w6 w7 w8)
wordsToIp _ = Nothing

add4 :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
add4 w1 w2 w3 w4 = w1 * base ^ 3 + w2 * base ^ 2 + w3 * base + w4
  where
    base = 16 ^ 4

parseIPv6 :: Parser IPAddress6
parseIPv6 = do
  groups <- parseGroups
  let mip = wordsToIp (uncollapse groups)
  case mip of
    Just ip -> return ip
    _       -> fail ""

toIPAddress6 :: IPAddress -> IPAddress6
toIPAddress6 (IPAddress w) = IPAddress6 0 (fromIntegral w)

testIP6 :: IO ()
testIP6 =
  hspec $ do
    describe "IPv6" $ do
      it "parses simple case" $ do
        maybeSuccess (parseString parseIPv6 mempty "0:0:0:0:0:ffff:ac10:fe01") `shouldBe`
          Just ((IPAddress6 0 281473568538113))
        maybeSuccess (parseString parseIPv6 mempty "0:0:0:0:0:ffff:cc78:f") `shouldBe`
          Just ((IPAddress6 0 281474112159759))
      it "handles collapse" $ do
        maybeSuccess
          (parseString
             parseIPv6
             mempty
             "FE80:0000:0000:0000:0202:B3FF:FE1E:8329") `shouldBe`
          maybeSuccess
            (parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329")
      it "shows integer value" $ do
        show <$>
          maybeSuccess
            (parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329") `shouldBe`
          Just "338288524927261089654163772891438416681"
      it "can convert from IPv4" $ do
        toIPAddress6 (IPAddress 12345) `shouldBe` (IPAddress6 0 12345)
