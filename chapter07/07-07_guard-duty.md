# Exercises: Guard Duty

```haskell
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100
```

1. It is probably clear to you why you wouldn’t put an otherwise in your top-most guard, but try it with `avgGrade` anyway and see what happens. It’ll be more clear if you rewrite it as an otherwise match: `| otherwise = 'F'`. What happens now if you pass a 90 as an argument? 75? 60?

    Answer: `otherwise` is always `True`, so the topmost guard will always be chosen. `avgGrade` will always return `F`.

2. What happens if you take `avgGrade` as it is written and reorder the guards? Does it still typecheck and work the same? Try moving `| y >= 0.7 = 'C'` and passing it the argument 90, which should be an ‘A.’ Does it return an ‘A’?

    Answer: The order matters. `y >= 0.7` will always be true if `y >= 0.9` ord `y >= 0.8` is true. If this guard is moved ahead of those, the latter guards can never be reached.

3. The following function returns

    ```haskell
    pal xs
        | xs == reverse xs = True
        | otherwise = False
    ```

a) `xs` written backwards when it’s `True`
b) `True` when `xs` is a palindrome
c) `False` when `xs` is a palindrome
d) `False` when `xs` is reversed

Answer: b)

4. What types of arguments can pal take?
    Answer: `xs :: [a]`, any list.

5. What is the type of the function pal?
    Answer: `pal :: [a] -> Bool`

6. The following function returns

    ```haskell
    numbers x
        | x < 0 = -1
        | x == 0 = 0
        | x > 0 = 1
    ```

    a) the value of its argument plus or minus 1
    b) the negation of its argument
    c) an indication of whether its argument is a positive or nega-
    tive number or zero
    d) binary machine language

    Answer: c)

7. What types of arguments can numbers take?
    Answer: Any number that can be ordered, `x :: (Ord a, Num a) -> a`

8. What is the type of the function numbers?
    Answer: `numbers :: (Ord a, Num a, Num b) => a -> b`