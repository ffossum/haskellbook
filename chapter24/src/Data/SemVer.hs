module Data.SemVer where

import           Control.Applicative
import           Data.List
import           Test.Hspec
import           Text.Trifecta

import           MaybeSuccess

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Show, Eq)

instance Ord NumberOrString where
  compare (NOSS _) (NOSI _)   = GT
  compare (NOSI _) (NOSS _)   = LT
  compare (NOSS s1) (NOSS s2) = compare s1 s2
  compare (NOSI i1) (NOSI i2) = compare i1 i2

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer =
  SemVer Major
         Minor
         Patch
         Release
         Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare (SemVer major1 minor1 patch1 rel1 _) (SemVer major2 minor2 patch2 rel2 _) =
    maybe EQ id result
    where
      result = find (/= EQ) comparisons
      comparisons =
        [ compare major1 major2
        , compare minor1 minor2
        , compare patch1 patch2
        , compareRelease rel1 rel2
        ]

compareRelease :: Release -> Release -> Ordering
compareRelease [] [] = EQ
compareRelease [] _  = GT
compareRelease _ []  = LT
compareRelease r1 r2 = compare r1 r2

decimalNoLeadingZeros :: Parser Integer
decimalNoLeadingZeros = do
  firstDigit <- digit
  case firstDigit of
    '0' -> return 0 <* notFollowedBy digit <|> fail "leading zeros not allowed"
    _ -> do
      digits <- many digit
      return (read (firstDigit : digits))

parseIdentifier :: Parser NumberOrString
parseIdentifier =
  (NOSS <$> try (stringIdentifier <* notFollowedBy alphaNum)) <|>
  (NOSI <$> (decimalNoLeadingZeros <* notFollowedBy alphaNum))

stringIdentifier :: Parser String
stringIdentifier = do
  a1 <- many digit
  a2 <- some letter
  a3 <- many alphaNum
  return (a1 ++ a2 ++ a3)

parseIdentifiers :: Parser [NumberOrString]
parseIdentifiers = do
  firstIdentifier <- parseIdentifier
  identifiers <- many (char '.' *> parseIdentifier)
  return (firstIdentifier : identifiers)

parseRelease :: Parser Release
parseRelease = do
  identifiers <- optional (char '-' *> parseIdentifiers)
  return $ maybe [] id identifiers

parseMetadata :: Parser Metadata
parseMetadata = do
  identifiers <- optional (char '+' *> parseIdentifiers)
  return $ maybe [] id identifiers

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimalNoLeadingZeros
  char '.'
  minor <- decimalNoLeadingZeros
  char '.'
  patch <- decimalNoLeadingZeros
  rel <- parseRelease
  meta <- parseMetadata
  return (SemVer major minor patch rel meta)

testSemVer :: IO ()
testSemVer =
  hspec $ do
    describe "SemVer" $ do
      it "parser" $ do
        maybeSuccess (parseString parseSemVer mempty "2.1.1") `shouldBe`
          Just (SemVer 2 1 1 [] [])
        maybeSuccess (parseString parseSemVer mempty "1.0.0-x.7.z.92") `shouldBe`
          Just (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
      it "Ord instance" $ do
        SemVer 2 1 1 [] [] > SemVer 2 1 0 [] [] `shouldBe` True
        SemVer 1 0 0 [] [] >
          SemVer 1 0 0 [(NOSS "rc"), (NOSI 1)] [] `shouldBe` True
      it "Metadata does not matter for Ord" $ do
        compare (SemVer 1 0 0 [] [NOSI 1]) (SemVer 1 0 0 [] [NOSI 2]) `shouldBe`
          EQ
