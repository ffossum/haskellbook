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

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = a == x || myElem a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a = myAny (==a)

myReverse :: [a] -> [a]
myReverse f = go f []
    where
        go [] result = result
        go (x:xs) result = go xs (x:result)

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go xs x
    where
        go [] res = res
        go (x:xs) res = case (f x res) of
            GT -> go xs x
            _  -> go xs res

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go xs x
    where
        go [] res = res
        go (x:xs) res = case (f x res) of
            LT -> go xs x
            _  -> go xs res

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

