module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (a:_) = Just a

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_:as) = Just as
