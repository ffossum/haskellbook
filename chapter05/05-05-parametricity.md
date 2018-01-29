# Parametricity

2. Write both possible versions of `a -> a -> a`.

    Answer:
    ```haskell
    version1 :: a -> a -> a
    version1 a1 a2 = a1

    version2 :: a -> a -> a
    version2 a1 a2 = a2
    ```

3. Implement `a -> b -> b`.

    Answer: Only one possible implementation:
    ```haskell
    f :: a -> b -> b
    f a b = b
    ```
