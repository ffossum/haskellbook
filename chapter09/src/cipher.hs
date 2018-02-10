module Cipher where

import Data.List (elemIndex, find)

lowers :: [Char]
lowers = ['a'..'z']

uppers :: [Char]
uppers = ['A'..'Z']

digits :: [Char]
digits = ['0'..'9']

alphabets :: [[Char]]
alphabets = [lowers, uppers, digits]

getAlphabet :: Char -> Maybe [Char]
getAlphabet c = find (elem c) alphabets

shift :: (Enum a, Eq a) => [a] -> Int -> a -> a
shift alphabet shiftN c = maybe c id newC
    where
        newC = do {
            index <- elemIndex c alphabet;
            newIndex <- return $ (index + shiftN) `mod` (length alphabet);
            return $ alphabet !! newIndex;
        }

caesarChar :: Int -> Char -> Char
caesarChar shiftN c = case getAlphabet c of
    Just alpha -> shift alpha shiftN c
    Nothing -> c

caesar :: Int -> String -> String
caesar shiftN = map (caesarChar shiftN)

unCaesar :: Int -> String -> String
unCaesar shiftN = caesar (-shiftN)


-- Vigenere

getShiftN :: Char -> Int
getShiftN keyChar = maybe 0 id result
    where
        result = do {
            alpha <- getAlphabet keyChar;
            elemIndex keyChar alpha;
        }

vigenereChar :: Char -> Char -> Char
vigenereChar keyChar = caesarChar shiftN
    where
        shiftN = getShiftN keyChar

unvigenereChar :: Char -> Char -> Char
unvigenereChar keyChar = caesarChar (-shiftN)
    where
        shiftN = getShiftN keyChar

vigenere :: String -> String -> String
vigenere keyword = zipWith vigenereChar repeatedKeyword
    where
        repeatedKeyword = concat $ repeat keyword

unvigenere :: String -> String -> String
unvigenere keyword = zipWith unvigenereChar repeatedKeyword
    where
        repeatedKeyword = concat $ repeat keyword