module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

fibonacci :: Integral a => [a]
fibonacci = [0,1,1] ++ zipWith (+) as bs
    where
        as = drop 1 fibonacci
        bs = drop 1 as

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


sumN :: Integral a => a -> a
sumN n = sum [1..n]

mult :: (Integral a) => a -> a -> a
mult x y = go 0 x y
    where
        go product x y
            | x == 0 = product
            | x > 0  = go (product + y) (x - 1) y
            | x < 0  = go (product - y) (x + 1) y

data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy 0 _ = Result 0
dividedBy num denom
    | num > 0 && denom > 0 = Result $ answer
    | num < 0 && denom < 0 = Result $ answer
    | otherwise            = Result $ negate answer
    where
        (answer, _) = go (abs num) (abs denom) 0
        go n d count
            | n < d = (count, n)
            | otherwise =
                go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 n
    | n > 100   = n - 10
    | otherwise = mc91(mc91(n + 11))
