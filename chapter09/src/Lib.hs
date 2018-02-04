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

threes = length . filter (\x -> rem x 3 == 0) $ [1..30]

isArticle :: String -> Bool
isArticle = (`elem` ["the", "a", "an"])

myFilter :: String -> [String]
myFilter = (filter (not . isArticle)) . words



myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:as) (b:bs) = (a,b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = (f a b) : myZipWith f as bs

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith (,)