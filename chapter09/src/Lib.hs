module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (a:_) = Just a

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_:as) = Just as

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
tuples = [ (x,y) | x <- mySqr, y <- myCube ]
smallTuples = [ (x,y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]