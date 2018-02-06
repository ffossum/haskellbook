# Scans exercises

```haskell
fibs = 1 : scanl (+) 1 fibs
```

1. Modify your fibs function to only return the first 20 Fibonacci numbers.

    Answer:
    ```haskell
    fibs = take 20 $ 1 : scanl (+) 1 fibs
    ```

2. Modify fibs to return the Fibonacci numbers that are less than 100.

    Answer:
    ```haskell
    fibs = takeWhile (<100) $ 1 : scanl (+) 1 fibs
    ```


3. Try to write the `factorial` function from Recursion as a scan. You’ll want `scanl` again, and your start value will be 1.

    Warning: this will also generate an infinite list, so you may want to pass it through a take function or similar.

    Answer:
    ```haskell
    factorial = scanl (*) 1 [1..]
    ```


