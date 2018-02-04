# Zipping exercises

1. Write your own version of `zip` and ensure it behaves the same as the original.

    ```haskell
    zip :: [a] -> [b] -> [(a, b)]
    zip = undefined
    ```

    Answer:
    ```haskell
    myZip :: [a] -> [b] -> [(a, b)]
    myZip [] _ = []
    myZip _ [] = []
    myZip (a:as) (b:bs) = (a,b) : myZip as bs
    ```

2. Do what you did for `zip`, but now for `zipWith`:
    ```haskell
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith = undefined
    ```
    Answer:
    ```haskell
    myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    myZipWith _ [] _ = []
    myZipWith _ _ [] = []
    myZipWith f (a:as) (b:bs) = (f a b) : myZipWith f as bs
    ```

3. Rewrite your `zip` in terms of the `zipWith` you wrote.

    Answer:
    ```haskell
    myZip2 :: [a] -> [b] -> [(a, b)]
    myZip2 = myZipWith (,)
    ```