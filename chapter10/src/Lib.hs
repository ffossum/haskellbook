module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

a = foldr (flip const) 'a' [1..5]

b = foldr (flip const) 0 "tacos"