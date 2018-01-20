# Chapter Exercises

## Parenthesization

Parenthesize the following expressions more explicitly with-
out changing their results.

1. `2 + 2 * 3 - 1`
    `2 + (2 * 3) - 1`

2. `(^) 10 $ 1 + 1`
    `(^) 10 $ (1 + 1)`
    `10 ^ (1 + 1)`

3. `2 ^ 2 * 4 ^ 5 + 1`
    `(2 ^ 2) * (4 ^ 5) + 1`
    `((2 ^ 2) * (4 ^ 5)) + 1`

## Equivalent expressions

Which of the following pairs of expressions will return the same
result when evaluated?

1. `1 + 1`
    `2`
    Same result.

2. `10 ^ 2`
    `10 + 9 * 10`
    Same result. `(10 * 10) == (10 + 90) == 100`

3. `400 - 37`
    `(-) 37 400`
    Not the same. Second expression is `37 - 400`.

4.  ``100 `div` 3``
    `100 / 3`
    Not the same. First expression is integral (`33`), second is fractional (`33.333...`).

5.  `2 * 5 + 18`
    `2 * (5 + 18)`
    Not the same. First expression is `(2 * 5) + 18`.

## More fun with functions

Rewrite it such that it could be evaluated in the REPL.
```haskell
z = 7
x = y ^ 2
waxOn = x * 5
y = z + 8
```

Answer:
```haskell
z = 7
y = z + 8
x = y ^ 2
waxOn = x * 5
```

4. Rewrite `waxOn` as an expression with a where clause in your source file.

Answer:
```haskell
waxOn = x * 5
    where
        z = 7
        y = z + 8
        x = y ^ 2
```

