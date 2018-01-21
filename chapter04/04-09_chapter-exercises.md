# Chapter Exercises

```haskell
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
```

`length` is a function that takes a list and returns a result that tells how many items are in the list.

1. Given the definition of length above, what would the type signa-
ture be? How many arguments, of what type does it take? What
is the type of the result it evaluates to?

    Answer:
    ```haskell
    length :: [a] -> Int
    ```
    It would take one argument, a list, and return an `Int`.


2. What are the results of the following expressions?

    a) length [1, 2, 3, 4, 5]
    Answer: 5

    b) length [(1, 2), (2, 3), (3, 4)]
    Answer: 3

    c) length allAwesome
    Answer: 2

    d) length (concat allAwesome)
    Answer: 5

3. One works and one returns an error. Determine which will return an error and why.

    ```haskell
    Prelude> 6 / 3
    -- and
    Prelude> 6 / length [1, 2, 3]
    ```
    The second expression fails. `/` requires `Fractional` operand, but `length [1, 2, 3]` is an `Int`.

4. How can you fix the broken code from the preceding exercise using a different division function/operator?

    Answer:

    ```haskell
    6 `div` length [1,2,3]
    ```

5. What is the type of the expression `2 + 3 == 5`? What would we expect as a result?

    Answer: `True`. `+` has presedence over `==`, so the expression evaluates as `(2 + 3) == 5`.

6. What is the type and expected result value of the following:

    ```haskell
    Prelude> let x = 5
    Prelude> x + 3 == 5
    ```

    Answer: `False :: Bool`

7. Below are some bits of code. Which will work? Why or why not? If they will work, what value would these reduce to?

    ```haskell
    length allAwesome == 2
    ```
    Answer: `True`

    ```haskell
    length [1, 'a', 3, 'b']
    ```
    Will not work. All elements in a list must be of the same type.

    ```haskell
    length allAwesome + length awesome
    ```
    Answer: `5`

    ```haskell
    (8 == 8) && ('b' < 'a')
    ```
    Answer: `False`

    ```haskell
    (8 == 8) && 9
    ```
    Will not work. `&&` requires `Bool` operands, but `9` is not a `Bool`.

8. Write a function that tells you whether or not a given String (or
list) is a palindrome. Here you’ll want to use a function called
`reverse` a predefined function that does what it sounds like.

    ```haskell
    reverse :: [a] -> [a]
    reverse "blah"
    "halb"
    ```

    ```haskell
    isPalindrome :: (Eq a) => [a] -> Bool
    isPalindrome x = x == reverse x
    ```

9. Write a function to return the absolute value of a number using
`if-then-else`.

    ```haskell
    myAbs :: Integer -> Integer
    myAbs x = if (x > 0) then x else (-x)
    ```

10. Fill in the definition of the following function, using `fst` and `snd`:

    ```haskell
    f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    f x y = ((snd x, snd y), (fst x, fst y))
    -- f (a, b) (c, d) = ((b, d), (a, c))
    ```

## Correcting syntax

1. Here, we want a function that adds 1 to the length of a string
argument and returns that result.

    ```haskell
    x = (+)

    F xs = w 'x' 1
        where w = length xs
    ```

    Fixed:
    ```haskell
    x = (+)

    f xs = w `x` 1
        where w = length xs
    ```

2. This is supposed to be the identity function, `id`.
    ```haskell
    \X = x
    ```
    Fixed:
    ```haskell
    \x -> x
    ```

3. When fixed, this function will return `1` from the value `(1, 2)`.

```haskell
f (a b) = A
```
Fixed:
```haskell
f (a, b) = a
```

## Match the function names to their types

1. Which of the following types is the type of `show`?
a) `show a => a -> String`
b) `Show a -> a -> String`
c) `Show a => a -> String`
Answer: c)

2. Which of the following types is the type of `(==)`?
a) `a -> a -> Bool`
b) `Eq a => a -> a -> Bool`
c) `Eq a -> a -> a -> Bool`
d) `Eq a => A -> Bool`
Answer: b)

3. Which of the following types is the type of `fst`?
a) `(a, b) -> a`
b) `b -> a`
c) `(a, b) -> b`
Answer: a)

4. Which of the following types is the type of `(+)`?
a) `(+) :: Num a -> a -> a -> Bool`
b) `(+) :: Num a => a -> a -> Bool`
c) `(+) :: num a => a -> a -> a`
d) `(+) :: Num a => a -> a -> a`
e) `(+) :: a -> a -> a`
Answer: d)
