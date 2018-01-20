# Comprehension Check

1. Given the following lines of code as they might appear in a source file, how would you change them to use them directly in the REPL?

    ```haskell
    half x = x / 2

    square x = x * x
    ```

    Answer:

    In GCHi versions < 8 I would have to prepend the declarations with `let`. For later versions no change is needed.

2. Write one function that has one parameter and works for all the following expressions. Be sure to name the function.

    ```haskell
    3.14 * (5 * 5)
    3.14 * (10 * 10)
    3.14 * (2 * 2)
    3.14 * (4 * 4)
    ```

    Answer:

    ```haskell
    circleArea r = 3.14 * (r * r)
    ```

3. There is a value in Prelude called pi. Rewrite your function to use pi instead of 3.14.

    Answer:
    ```haskell
    circleArea r = pi * r * r
    ```

