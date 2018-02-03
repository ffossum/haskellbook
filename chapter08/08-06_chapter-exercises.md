# Chapter Exercises

## Review of types
(Correct answer in bold)

1. What is the type of `[[True, False], [True, True], [False, True]]`?

    a) `Bool`
    b) mostly `True`
    c) `[a]`
    **d) `[[Bool]]`**

2. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?

    a) `[(True, False), (True, True), (False, True)]`
    **b) `[[3 == 3], [6 > 5], [3 < 4]]`** _The type is `[[Bool]]` for both, even though the lengths of the inner lists are different._
    c) `[3 == 3, 6 > 5, 3 < 4]`
    d) `["Bool", "more Bool", "Booly Bool!"]`

3. For the following function
    ```haskell
    func :: [a] -> [a] -> [a]
    func x y = x ++ y
    ```

    which of the following is true?

    a) x and y must be of the same type
    b) x and y must both be lists
    c) if x is a String then y must be a String
    **d) all of the above**

4. For the func code above, which is a valid application of func to both of its arguments?

    a) `func "Hello World"`
    **b) `func "Hello" "World"`**
    c) `func [1, 2, 3] "a, b, c"`
    d) `func ["Hello", "World"]`

## Reviewing curring

Given the following definitions, tell us what value results from further applications.

```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"
```

With types:
```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
```

1. What is the value of `appedCatty "woohoo!"`? Try to determine the
answer for yourself, then test in the REPL.
    Answer:
    `cattyConny "woops" "woohoo!"`
    **`"woops mrow woohoo!"`**

2. `frappe "1"`
    Answer:
    `flippy "haha" "1"`
    `cattyConny "1" "haha"`
    **`"1 mrow haha"`**
3. `frappe (appedCatty "2")`
    Answer:
    `frappe (cattyConny "woops" "2")`
    `frappe "woops mrow 2"`
    **`"woops mrow 2 mrow haha"`**

4. `appedCatty (frappe "blue")`
    Answer:
    `appedCatty "blue mrow haha"`
    **`"woops mrow blue mrow haha"`**
5.
```haskell
cattyConny (frappe "pink")
           (cattyConny "green" (appedCatty "blue"))
```
Answer:
```haskell
cattyConny "pink mrow haha"
           (cattyConny "green" "woops mrow blue")
```
```haskell
cattyConny "pink mrow haha"
           "green mrow woops mrow blue"
```
**`"pink mrow haha mrow green mrow woops mrow blue"`**

6. `cattyConny (flippy "Pugs" "are") "awesome"`
Answer:
`cattyConny (cattyConny "are" "Pugs") "awesome"`
`cattyConny "are mrow Pugs" "awesome"`
**`"are mrow Pubs mrow awesome"`**

## Recursion
```haskell
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n   d count
           | n < d = (count, n)
           | otherwise =
                go (n - d) d (count + 1)
```
1. Write out the steps for reducing `dividedBy 15 2` to its final answer
according to the Haskell code.
Answer:

    `dividedBy 15 2`
    `go 15 2 0`
    `go 13 2 1`
    `go 11 2 2`
    `go 9  2 3`
    `go 7  2 4`
    `go 5  2 5`
    `go 3  2 6`
    `go 1  2 7`
    **`(7, 1)`**

2. Write a function that recursively sums all numbers from 1 to n, n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 + 5 to get 15. The type should be `(Eq a, Num a) => a -> a`.

    Answer:
    ```haskell
    sumN :: (Eq a, Num a) => a -> a
    sumN n = go 0 n
        where
            go total n
                | n == 0 = total
                | otherwise = go (total + n) (n - 1)
    ```

    This function recurs forever if n is a negative number, or not an integer.
    Better version:
    ```haskell
    sumN :: Integral a => a -> a
    sumN n = sum [1..n]
    ```

3. Write a function that multiplies two integral numbers using recursive summation. The type should be `(Integral a) => a -> a -> a`.

    ```haskell
    mult :: (Integral a) => a -> a -> a
    mult x y = go 0 x y
        where
            go product x y
                | x == 0 = product
                | x > 0  = go (product + y) (x - 1) y
                | x < 0  = go (product - y) (x + 1) y
    ```

## Fixing `dividedBy`

Our `dividedBy` function wasn’t quite ideal. For one thing. It was a partial function and doesn’t return a result (bottom) when given a divisor that is 0 or less.

Using the pre-existing `div` function we can see how negative numbers should be handled:
```
Prelude> div 10 2
5
Prelude> div 10 (-2)
-5
Prelude> div (-10) (-2)
5
Prelude> div (-10) (2)
-5
```
The next issue is how to handle zero. Zero is undefined for division in math, so we ought to use a datatype that lets us say there was no sensible result when the user divides by zero. If you need inspiration, consider using the following datatype to handle this.

```haskell
data DividedResult = Result Integer | DividedByZero
```

Answer:
```haskell
data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy 0 _ = Result 0
dividedBy num denom
    | num > 0 && denom > 0 = Result $ answer
    | num < 0 && denom < 0 = Result $ answer
    | otherwise            = Result $ negate answer
    where
        (answer, _) = go (abs num) (abs denom) 0
        go n d count
            | n < d = (count, n)
            | otherwise =
                go (n - d) d (count + 1)
```

## McCarthy 91 function

Your task is to write the function in Haskell. The McCarthy 91 function yields x − 10 when x > 100 and 91 otherwise. The function is recursive.

```
MC(n) = | n - 10            if n >  100
        | MC(MC(n + 11))    if n <= 100
```

Answer:
```haskell
mc91 :: Integral a => a -> a
mc91 n
    | n > 100   = n - 10
    | otherwise = mc91(mc91(n + 11))
```

## Numbers into words

```haskell
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = undefined

digits :: Int -> [Int]
digits n = undefined

wordNumber :: Int -> String
wordNumber n = undefined
```

Fill in the implementations of the functions above so that wordNumber
returns the English word version of the `Int` value.

Answer:
[WordNumber.hs](./src/WordNumber.hs)
