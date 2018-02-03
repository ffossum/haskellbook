# Artful dodgy

Given the following definitions tell us what value results from further applications. When you’ve written down at least some of the answers and think you know what’s what, type the definitions into a file and load them in GHCi to test your answers.

```haskell
-- Types not provided,
-- try filling them in yourself.

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
```

With types:
```haskell
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2
```

1. For example, given the expression `dodgy 1 0`, what do you think will happen if we evaluate it? If you put the definitions in a file and load them in GHCi, you can do the following to see the result.

    ```
    Prelude> dodgy 1 0
    1
    ```

    Now attempt to determine what the following expressions reduce to. Do it in your head, verify in your REPL after you think you have an answer.

2. `dodgy 1 1` Answer: `11`
3. `dodgy 2 2` Answer: `22`
4. `dodgy 1 2` Answer: `21`
5. `dodgy 2 1` Answer: `12`
6. `oneIsOne 1` Answer: `11`
7. `oneIsOne 2` Answer: `21`
8. `oneIsTwo 1` Answer: `21`
9. `oneIsTwo 2` Answer: `22`
10. `oneIsOne 3` Answer: `31`
11. `oneIsTwo 3` Answer: `23`
