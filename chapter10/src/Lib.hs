module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

a = foldr (flip const) 'a' [1..5]

b = foldr (flip const) 0 "tacos"

fibs = take 20 $ 1 : scanl (+) 1 fibs

fact = scanl (*) 1 [1..]

stops = "pbtdkg"
vowels = "aeiou"

combinations = [ [a,b,c] | a <- stops, b <- vowels, c <- stops ]

sentences nouns verbs = [
    a ++ " " ++ b ++ " " ++ c |
    a <- nouns, b <- verbs, c <- nouns
    ]


seekritFunc x =
    (fromIntegral totalWordLength) / (fromIntegral numberOfWords)
    where
        ws = words x
        totalWordLength = sum (map length ws)
        numberOfWords = length ws