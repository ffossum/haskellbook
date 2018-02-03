module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = (\n -> n + 1)

addFive = (\x y -> (if x > y then y else x) + 5)

newtype Username =
    Username String
newtype AccountNumber =
    AccountNumber Integer
data User =
    UnregisteredUser
    | RegisteredUser Username AccountNumber

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f :: (a, b, c)
        -> (d, e, f)
        -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

functionC x y = case (x > y) of
    True  -> x
    False -> y

ifEvenAdd2 n = case (even n) of
    True  -> n + 2
    False -> n


dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | otherwise = 'F'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1

tensDigitOriginal :: Integral a => a -> a
tensDigitOriginal x = d
    where
        xLast = x `div` 10
        d = xLast `mod` 10

tensDigit :: Integral a => a -> a
tensDigit x = tens
    where
        (d, _) = x `divMod` 10
        (_, tens) = d `divMod` 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = hundreds
    where
        (d, ones) = x `divMod` 10
        (d2, tens) = d `divMod` 10
        (d3, hundreds) = d2 `divMod` 10

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase a1 a2 b = case b of
    False -> a1
    True  -> a2

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard a1 a2 b
    | b         = a2
    | otherwise = a1

g :: (a -> b) -> (a, c) -> (b, c)
g ab (a, c) = (ab a, c)

