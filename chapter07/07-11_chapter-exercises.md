# Chapter Exercises

## Multipe choice

(Correct answer in bold)

1. A polymorphic function
a) changes things into sheep when invoked
b) has multiple arguments
c) has a concrete type
**d) may resolve to values of different types, depending on inputs**

2. Two functions named `f` and `g` have types `Char -> String` and `String -> [String]` respectively. The composed function `g . f` has the type
a) `Char -> String`
**b) `Char -> [String]`**
c) `[[String]]`
d) `Char -> String -> [String]`

3. A function `f` has the type `Ord a => a -> a -> Bool` and we apply it to one numeric value. What is the type now?
a) `Ord a => a -> Bool`
b) `Num -> Num -> Bool`
c) `Ord a => a -> a -> Integer`
**d) `(Ord a, Num a) => a -> Bool`**

4. A function with the type `(a -> b) -> c`
a) requires values of three different types
**b) is a higher-order function**
c) must take a tuple as its first argument
d) has its parameters in alphabetical order

5. Given the following definition of f, what is the type of `f True`?
    ```haskell
    f :: a -> a
    f x = x
    ```
    **a) `f True :: Bool`**
    b) `f True :: String`
    c) `f True :: Bool -> Bool`
    d) `f True :: a`

## Let's write code

1. The following function returns the tens digit of an integral argument.

    ```haskell
    tensDigit :: Integral a => a -> a
    tensDigit x = d
        where
            xLast = x `div` 10
            d = xLast `mod` 10
    ```

    a) First, rewrite it using divMod.
    Answer:
    ```haskell
    tensDigit :: Integral a => a -> a
    tensDigit x = tens
        where
            (d, ones) = x `divMod` 10
            (_, tens) = d `divMod` 10
    ```

    b) Does the divMod version have the same type as the original version?
        Answer: Well, yes... I thought that was a requirement when rewriting the function.

    c) Next, let’s change it so that we’re getting the hundreds digit instead. You could start it like this (though that may not be the only possibility):
    ```haskell
    hunsD x = d2
        where d = undefined
        ...
    ```
    Answer:
    ```haskell

    hundredsDigit :: Integral a => a -> a
    hundredsDigit x = hundreds
        where
            (d, ones) = x `divMod` 10
            (d2, tens) = d `divMod` 10
            (d3, hundreds) = d2 `divMod` 10
    ```

2. Implement the function of the type `a -> a -> Bool -> a` once each using a case expression and once with a guard.
    ```haskell
    foldBool :: a -> a -> Bool -> a
    foldBool =
        error
        "Error: Need to implement foldBool!"
    ```
    The result is semantically similar to if-then-else expressions but syntactically quite different. Here is the pattern matching version to get you started:

    ```haskell
    foldBool3 :: a -> a -> Bool -> a
    foldBool3 x _ False = x
    foldBool3 _ y True = y
    ```

    Answer:
    ```haskell
    foldBoolCase :: a -> a -> Bool -> a
    foldBoolCase a1 a2 b = case b of
        False -> a1
        True  -> a2

    foldBoolGuard :: a -> a -> Bool -> a
    foldBoolGuard a1 a2 b
        | b         = a2
        | otherwise = a1
    ```

3. Fill in the definition. Note that the first argument to our function is also a function which can be applied to values. Your second argument is a tuple, which can be used for pattern matching:

    ```haskell
    g :: (a -> b) -> (a, c) -> (b, c)
    g = undefined
    ```
    Answer:
    ```haskell
    g :: (a -> b) -> (a, c) -> (b, c)
    g ab (a, c) = (ab a, c)
    ```


4. For this next exercise, you’ll experiment with writing pointfree versions of existing code. This involves some new information, so read the following explanation carefully. Typeclasses are dispatched by type. `Read` is a typeclass like `Show`, but it is the dual or “opposite” of `Show`. In general, the `Read` typeclass isn’t something you should plan to use a lot, but this exercise is structured to teach you something about the interaction between typeclasses and types. The function `read` in the `Read` typeclass has the type:

```haskell
read :: Read a => String -> a
```
Notice a pattern?
```haskell
read :: Read a => String -> a
show :: Show a => a -> String
```

Write the following code into a source file. Then load it and run it in GHCi to make sure you understand why the evaluation results in the answers you see.

```haskell
-- arith4.hs
module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
    print (roundTrip 4)
    print (id 4)
```

5. Next, write a pointfree version of roundTrip. (n.b., This refers to
the function definition, not to its application in main.)
    Answer:
    ```haskell
    roundTrip :: (Show a, Read a) => a -> a
    roundTrip = read . show
    ```

6. We will continue to use the code in `module Arith4` for this exercise as well.

    When we apply `show` to a value such as `(1 :: Int)`, the `a` that implements `Show` is `Int`, so GHC will use the `Int` instance of the `Show` typeclass to stringify our `Int` of 1.

    However, `read` expects a `String` argument in order to return an a. The `String` argument that is the first argument to read tells the function nothing about what type the de-stringified result should be. In the type signature `roundTrip` currently has, it knows because the type variables are the same, so the type that is the input to `show` has to be the same type as the output of `read`.

    Your task now is to change the type of roundTrip in `Arith4` to `(Show a, Read b) => a -> b`. How might we tell GHC which instance of `Read` to dispatch against the `String` now? Make the expression `print (roundTrip 4)` work. You will only need the _has the type_ syntax of `::` and parentheses for scoping.

    Answer:
    We must add the type tothe function call:
    ```haskell
    roundTrip :: (Show a, Read b) => a -> b
    roundTrip a = read (show a)

    main = do
        print (roundTrip 4 :: Int)
    ```