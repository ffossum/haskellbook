{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Log where

import           Control.Applicative
import           Data.List
import           Data.Text           (pack, strip, unpack)
import           Data.Time
import           MaybeSuccess
import           Test.QuickCheck
import           Text.RawString.QQ
import           Text.Trifecta

newtype Activity =
  Activity (TimeOfDay, String)
  deriving (Eq, Ord)

alphaNumerals :: [Char]
alphaNumerals = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

instance Arbitrary Activity where
  arbitrary = Activity <$> ((,) <$> timeOfDay <*> description)
    where
      timeOfDay = do
        hh <- abs <$> arbitrary
        mm <- abs <$> arbitrary
        return (TimeOfDay hh mm 0)
      description = suchThat (listOf $ elements alphaNumerals) (not . null)

instance Show Activity where
  show (Activity (time, desc)) = hh ++ ":" ++ mm ++ " " ++ desc
    where
      (TimeOfDay h m _) = time
      hh = leftPadZero2 (show h)
      mm = leftPadZero2 (show m)

leftPadZero2 :: String -> String
leftPadZero2 []     = "00"
leftPadZero2 (x:[]) = '0' : x : []
leftPadZero2 x      = x

data DayEntry =
  DayEntry Day
           [Activity]
  deriving (Eq)

instance Show DayEntry where
  show (DayEntry day activities) = header ++ activityLines
    where
      header = "# " ++ (show day) ++ "\n"
      activityLines = intercalate "\n" $ map show activities

instance Arbitrary DayEntry where
  arbitrary = DayEntry <$> arbitraryDay <*> (sort <$> activities)
    where
      arbitraryDay = do
        y <- suchThat arbitrary (\a -> a > 0 && a < 3000)
        m <- suchThat arbitrary (\a -> a > 0 && a <= 12)
        d <- suchThat arbitrary (\a -> a > 0 && a <= 31)
        return (fromGregorian y m d)
      activities = sort <$> (suchThat arbitrary (\a -> length a > 0))

newtype Diary =
  Diary [DayEntry]
  deriving (Eq)

instance Arbitrary Diary where
  arbitrary = Diary <$> entries
    where
      entries = suchThat arbitrary (\a -> length a > 0 && length a < 10)

instance Show Diary where
  show (Diary entries) = intercalate "\n\n" $ map show entries

commentToken :: Parser ()
commentToken =
  token $ do
    string "--"
    whiteSpace
    many (noneOf "\n")
    return ()

parseDayEntry :: Parser DayEntry
parseDayEntry = do
  day <- parseHeader
  activities <- some parseActivity
  return $ DayEntry day activities

parseHeader :: Parser Day
parseHeader = do
  string "# "
  y <- decimal
  char '-'
  m <- fromInteger <$> decimal
  char '-'
  d <- fromInteger <$> decimal
  whiteSpace
  optional commentToken
  return $ fromGregorian y m d

parseActivity :: Parser Activity
parseActivity = do
  hh <- fromIntegral <$> decimal
  char ':'
  mm <- fromIntegral <$> decimal
  whiteSpace
  desc <-
    manyTill
      anyChar
      (try $ commentToken <|> try (string "\n" >> return ()) <|> eof)
  return $ Activity (TimeOfDay hh mm 0, (unpack . strip . pack) desc)

parseDiary :: Parser Diary
parseDiary = do
  optional (many commentToken)
  Diary <$> sepBy1 (token parseDayEntry) (optional commentToken)

diaryEx :: String
diaryEx =
  [r|-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

logSerializeProp :: Diary -> Bool
logSerializeProp d =
  maybeSuccess (parseString parseDiary mempty (show d)) == Just d

testLog :: IO ()
testLog = quickCheck logSerializeProp
