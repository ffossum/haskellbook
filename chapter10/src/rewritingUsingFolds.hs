module Rewriting where

-- Again, this type will be less
-- reusable than the one in GHC 7.10
-- and newer. Don't worry.
-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if x == False
    then False
    else myAnd xs

-- direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd xs

-- fold, not point-free
-- in the folding function
myAnd3 :: [Bool] -> Bool
myAnd3 = foldr
    (\a b ->
        if a == False
        then False
        else b) True

-- fold, both myAnd and the folding
-- function are point-free now
myAnd4 :: [Bool] -> Bool
myAnd4 = foldr (&&) True

-- 2. myAny returns True if a -> Bool applied to any of the values in the
-- list returns True.

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- 3. Write two versions of myElem. One version should use folding
-- and the other should use any.

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (==a)) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a = myAny (==a)

-- 4. Implement myReverse, don’t worry about trying to make it
-- lazy.

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5. Write myMap in terms of foldr. It should have the same behavior
-- as the built-in map.

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6. Write myFilter in terms of foldr. It should have the same behav-
-- ior as the built-in filter.

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []


-- 7. squish flattens a list of lists into a list

squish :: [[a]] -> [a]
squish = foldr (++) []


-- 8. squishMap maps a function over a list and concatenates the re-
-- sults.

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9. squishAgain flattens a list of lists into a list. This time re-use the
-- squishMap function.

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\a acc -> if (f a acc) == GT then a else acc)

-- 11. myMinimumBy takes a comparison function and a list and returns
-- the least element of the list based on the last value that the
-- comparison returned LT for.

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\a acc -> if (f a acc) == LT then a else acc)
