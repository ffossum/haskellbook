module EnumFromTo where

eft :: (Enum a, Ord a) => a -> a -> [a]
eft from to = reverse (go from to [])
    where
        go from to result
            | from <= to = go (succ from) to (from : result)
            | otherwise  = result

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
