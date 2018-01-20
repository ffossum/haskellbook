# Parantheses and Association

Below are some pairs of functions that are alike except for parenthe-
sization. Read them carefully and decide if the parentheses change
the results of the function. Check your work in GHCi.

1. a)
    ```haskell
    8 + 7 * 9`
    ```

    b)
    ```haskell
    (8 + 7) * 9
    ```

    Yes. a) is equivalent to
    ```haskell
    8 + (7 * 9)
    ```

2. a)
    ```haskell
    perimeter x y = (x * 2) + (y * 2)
    ```

    b)
    ```haskell
    perimeter x y = x * 2 + y * 2
    ```

    No, the two are the same.

3. a)
    ```haskell
    f x = x / 2 + 9
    ```

    b)
    ```haskell
    f x = x / (2 + 9)
    ```

    Yes. a) is equivalent to
    ```haskell
    f x = (x / 2) + 9
    ```
