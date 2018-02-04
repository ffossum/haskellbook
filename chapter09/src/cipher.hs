module Cipher where

import Data.List (elemIndex)

lowers :: [Char]
lowers = ['a'..'z']

uppers :: [Char]
uppers = ['A'..'Z']

digits :: [Char]
digits = ['0'..'9']

shift :: (Enum a, Eq a) => [a] -> Int -> a -> a
shift alphabet shiftN c =
    case newIndex of
        Nothing -> c
        Just i  -> alphabet !! i
    where
        index = elemIndex c alphabet
        newIndex = (`mod` (length alphabet)) <$> (+shiftN) <$> index

caesarChar :: Int -> Char -> Char
caesarChar shiftN c
    | elem c lowers = shift lowers shiftN c
    | elem c uppers = shift uppers shiftN c
    | elem c digits = shift digits shiftN c
    | otherwise     = c

caesar :: Int -> String -> String
caesar shiftN = map (caesarChar shiftN)

unCaesar :: Int -> String -> String
unCaesar shiftN = caesar (-shiftN)
