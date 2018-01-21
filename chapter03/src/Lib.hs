module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

letterIndex :: Int -> Char
letterIndex x = s !! x
        where s = "Curry is awesome!"