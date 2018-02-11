module ValidateWord where

import Data.Char

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels :: [Char]
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = (`elem` vowels) . toLower

mkWord :: String -> Maybe Word'
mkWord str
  | vowelCount < consonantCount = Just $ Word' str
  | otherwise                   = Nothing
  where
    vowelCount = length $ filter isVowel str
    consonantCount = length str - vowelCount
