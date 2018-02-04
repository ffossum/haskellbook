# Chapter Exercises

1. Query the types of isUpper and toUpper.

    ```haskell
    isUpper :: Char -> Bool

    toUpper :: Char -> Char
    ```

2. Given the following behaviors, which would we use to write a function that filters all the uppercase letters out of a String? Write that function such that, given the input `"HbEfLrLxO"`, your function will return `"HELLO"`.

    Answer:
    ```haskell
    filterUppers = filter isUpper
    ```

3. Write a function that will capitalize the first letter of a string and return the entire string. For example, if given the argument `"julie"`, it will return `"Julie"`.

    Answer:
    ```haskell
    capitalize :: String -> String
    capitalize "" = ""
    capitalize (h:t) = (toUpper h) : t
    ```

4. Now make a new version of that function that is recursive such that if you give it the input `"woot"`, it will holler back at you `"WOOT"`. The type signature won’t change, but you will want to add a base case.

    Answer:
    ```haskell
    capitalizeAll :: String -> String
    capitalizeAll "" = ""
    capitalizeAll (h:t) = (toUpper h) : capitalizeAll t
    ```

5. To do the final exercise in this section, we’ll need another standard function for lists called `head`. Query the type of `head` and experiment with it to see what it does. Now write a function that will capitalize the first letter of a `String` and return only that letter as the result.

    Answer:
    ```haskell
    capHead :: String -> Char
    capHead s = toUpper (head s)
    ```

6. Cool. Good work. Now rewrite it as a composed function. Then, for fun, rewrite it pointfree.

    Answer:
    ```haskell
    capHeadComposed :: String -> Char
    capHeadComposed s = toUpper . head $ s

    capHeadPointFree :: String -> Char
    capHeadPointFree = toUpper . head
    ```

## Ciphers

Caesar cipher: [cipher.hs](./src/cipher.hs)

## Writing your own standard functions

1. `myOr` returns `True` if any `Bool` in the list is `True`.

    Answer:
    ```haskell
    myOr :: [Bool] -> Bool
    myOr [] = False
    myOr (x:xs) = x || myOr xs
    ```

2. myAny returns `True` if `a -> Bool` applied to any of the values in the
list returns `True`.

    ```haskell
    myAny :: (a -> Bool) -> [a] -> Bool
    myAny = undefined
    ```
    Example for validating myAny:
    ```
    Prelude> myAny even [1, 3, 5]
    False
    Prelude> myAny odd [1, 3, 5]
    True
    ```

    Answer:
    ```haskell
    myAny :: (a -> Bool) -> [a] -> Bool
    myAny _ [] = False
    myAny f (x:xs) = f x || myAny f xs
    ```

3. After you write the recursive `myElem`, write another version that uses any. The built-in version of elem in GHC 7.10 and newer has a type that uses Foldable instead of the list type specifically. You can ignore that and write the concrete version that works only for list.

    ```haskell
    myElem :: Eq a => a -> [a] -> Bool
    ```

    ```
    Prelude> myElem 1 [1..10]
    True
    Prelude> myElem 1 [2..10]
    False
    ```

    Answer:
    ```haskell
    myElem :: Eq a => a -> [a] -> Bool
    myElem _ [] = False
    myElem a (x:xs) = a == x || myElem a xs

    myElem2 :: Eq a => a -> [a] -> Bool
    myElem2 a = myAny (==a)
    ```

4. Implement `myReverse`.
    ```haskell
    myReverse :: [a] -> [a]
    myReverse = undefined
    ```
    ```
    Prelude> myReverse "blah"
    "halb"
    Prelude> myReverse [1..5]
    [5,4,3,2,1]
    ```

    Answer:
    ```haskell
    myReverse :: [a] -> [a]
    myReverse f = go f []
        where
            go :: [a] -> [a] -> [a]
            go [] result = result
            go (x:xs) result = go xs (x:result)
    ```

5. `squish` flattens a list of lists into a list
    ```haskell
    squish :: [[a]] -> [a]
    squish = undefined
    ```
    Answer:
    ```haskell
    squish :: [[a]] -> [a]
    squish [] = []
    squish (x:xs) = x ++ squish xs
    ```

6. `squishMap` maps a function over a list and concatenates the results.
    ```haskell
    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap = undefined
    ```
    ```
    Prelude> squishMap (\x -> [1, x, 3]) [2]
    [1,2,3]
    Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
    "WO 1 HOO WO 2 HOO WO 3 HOO "
    ```
    Answer:
    ```haskell
    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap _ [] = []
    squishMap f (x:xs) = (f x) ++ (squishMap f xs)
    ```

7. `squishAgain` flattens a list of lists into a list. This time re-use the squishMap function.
    ```haskell
    squishAgain :: [[a]] -> [a]
    squishAgain = undefined
    ```
    Answer:
    ```haskell
    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id
    ```

8. `myMaximumBy` takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned `GT` for. If you import `maximumBy` from `Data.List`, you’ll see the type is:

```haskell
Foldable t => (a -> a -> Ordering) -> t a -> a
```
rather than

```haskell
(a -> a -> Ordering) -> [a] -> a
```

```haskell
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined
```
```
Prelude> let xs = [1, 53, 9001, 10]
Prelude> myMaximumBy compare xs
9001
```

Answer:
```haskell

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go xs x
    where
        go [] res = res
        go (x:xs) res = case (f x res) of
            GT -> go xs x
            _  -> go xs res
```


9. `myMinimumBy` takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.

```haskell
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined
```
```
Prelude> let xs = [1, 53, 9001, 10]
Prelude> myMinimumBy compare xs
1
```

Answer:
```haskell
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go xs x
    where
        go [] res = res
        go (x:xs) res = case (f x res) of
            LT -> go xs x
            _  -> go xs res
```

10. Using the `myMinimumBy` and `myMaximumBy` functions, write your own versions of maximum and minimum. If you have GHC 7.10 or newer, you’ll see a type constructor that wants a Foldable instance instead of a list as has been the case for many functions so far.

```haskell
myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined
```

Answer:
```haskell
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
```
