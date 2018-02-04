# Exercises: More Bottoms

As always, we encourage you to try figuring out the answers before you enter them into your REPL.

1. Will the following expression return a value or be ⊥?

    ```haskell
    take 1 $ map (+1) [undefined, 2, 3]
    ```
    Answer: Bottom

2. Will the following expression return a value?

    ```haskell
    take 1 $ map (+1) [1, undefined, 3]
    ```
    Answer: Yes, the result is `[2]`.

3. Will the following expression return a value?

    ```haskell
    take 2 $ map (+1) [1, undefined, 3]
    ```
    Answer: No, it will be bottom.

4. What does the following mystery function do? What is its type? Describe it (to yourself or a loved one) in standard English and then test it out in the REPL to make sure you were correct.

    ```haskell
    itIsMystery xs =
        map (\x -> elem x "aeiou") xs
    ```
    Answer:
    It takes a String, and returns a list of Bool, with a True value in every position that contained a lower case vowel.

5. What will be the result of the following functions:
    a)
    ```haskell
    map (^2) [1..10]
    ```
    Answer: `[1,4,9,16,25,36,49,64,81,100]`

    b)
    ```haskell
    map minimum [[1..10], [10..20], [20..30]]
    -- n.b. `minimum` is not the same function
    -- as the `min` that we used before
    ```
    Answer: `[1,10,20]`
    c)
    ```haskell
    map sum [[1..5], [1..5], [1..5]]
    ```
    Answer: `[15,15,15]`

6. Back in chapter 7, you wrote a function called `foldBool`. That function exists in a module known as `Data.Bool` and is called `bool`. Write a function that does the same (or similar, if you wish) as the `map` (`if-then-else`) function you saw above but uses `bool` instead of the `if-then-else` syntax. Your first step should be bringing the `bool` function into scope by typing import `Data.Bool` at your Prelude prompt.

    Answer: [foldBool.hs](./src/foldBool.hs)
