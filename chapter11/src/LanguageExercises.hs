module LanguageExercises where

import Data.Char
import Data.List
import Data.List.Split

capitalizeWords :: String -> [(String, String)]
capitalizeWords = (map f) . words
  where
    f x = (x, capitalizeWord x)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (h:t) = (toUpper h) : t

capitalizeParagraph :: String -> String
capitalizeParagraph = (intercalate ". ") . (map capitalizeWord) . (splitOn ". ")
