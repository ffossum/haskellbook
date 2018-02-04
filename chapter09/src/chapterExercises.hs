module ChapterExercises where

import Data.Char

filterUppers :: String -> String
filterUppers = filter isUpper

capitalize :: String -> String
capitalize "" = ""
capitalize (h:t) = (toUpper h) : t

capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll (h:t) = (toUpper h) : capitalizeAll t


capHead :: String -> Char
capHead s = toUpper (head s)



capHeadComposed :: String -> Char
capHeadComposed s = toUpper . head $ s

capHeadPointFree :: String -> Char
capHeadPointFree = toUpper . head