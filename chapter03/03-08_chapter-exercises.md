# Chapter Exercises

## Reading syntax

1. For the following lines of code, read the syntax carefully and
decide if they are written correctly. Test them in your REPL
after you’ve decided to check your work. Correct as many as
you can.

    a)
    ```haskell
    concat [[1, 2, 3], [4, 5, 6]]
    ```
    Correct.

    b)
    ```haskell
    ++ [1, 2, 3] [4, 5, 6]
    ```
    Fixed:
    ```haskell
    (++) [1, 2, 3] [4, 5, 6]
    ```


    c)
    ```haskell
    (++) "hello" " world"
    ```
    Correct.


    d)
    ```haskell
    ["hello" ++ " world]
    ```
    Fixed:
    ```haskell
    ["hello" ++ " world"]
    ```


    e)
    ```haskell
    4 !! "hello"
    ```
    Fixed:
    ```haskell
    "hello" !! 4
    ```


    f)
    ```haskell
    (!!) "hello" 4
    ```
    Correct.


    g)
    ```haskell
    take "4 lovely"
    ```
    Fixed:
    ```haskell
    take 4 "lovely"
    ```


    h)
    ```haskell
    take 3 "awesome"
    ```
    Correct.

2. Next we have two sets: the first set is lines of code and the other is a set of results. Read the code and figure out which results came from which lines of code. Be sure to test them in the REPL.

    a)
    ```haskell
    concat [[1 * 6], [2 * 6], [3 * 6]]
    ```
    Result: `[6, 12, 18]`

    b)
    ```haskell
    "rain" ++ drop 2 "elbow"
    ```
    Result: `"rainbow"`

    c)
    ```haskell
    10 * head [1, 2, 3]
    ```
    Result: `10`

    d)
    ```haskell
    (take 3 "Julie") ++ (tail "yes")
    ```
    Result: `"Jules"`

    e)
    ```haskell
    concat [tail [1, 2, 3],
            tail [4, 5, 6],
            tail [7, 8, 9]]
    ```
    Result: `[2, 3, 5, 6, 8, 9]`

## Building functions

3. Write a function of type `String -> Char` which returns the third
character in a `String`.

    ```haskell
    thirdLetter :: String -> Char
    thirdLetter x = x !! 2
    ```

4. Now change that function so the string operated on is always the same and the variable represents the number of the letter you want to return (you can use “Curry is awesome!” as your string input or a different string if you prefer).

    ```haskell
    letterIndex :: Int -> Char
    letterIndex x = s !! x
            where s = "Curry is awesome!"
    ```

5. Take the string `"Curry is awesome"` and return the result `"awesome is Curry"` using `take` and `drop`.

    ```haskell
    s1 = "Curry is awesome"

    s2 = (drop 9 s1) ++ (take 4 (drop 5 s1)) ++ (take 5 s1)
    ```

