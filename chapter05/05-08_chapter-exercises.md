# Chapter Exercises

## Multiple choice

1. A value of type `[a]` is
    a) a list of alphabetic characters
    b) a list of lists
    c) a list whose elements are all of some type `a`
    d) a list whose elements are all of different types
    Answer: c)

2. A function of type `[[a]] -> [a]` could
    a) take a list of strings as an argument
    b) transform a character into a string
    c) transform a string into a list of strings
    d) take two arguments
    Answer: a)

3. A function of type `[a] -> Int -> a`
    a) takes one argument
    b) returns one element of type `a` from a list
    c) must return an `Int` value
    d) is completely fictional
    Answer: b) (Although a) is also technically true, since all functions in haskell take one argument.)

4. A function of type `(a, b) -> a`
    a) takes a list argument and returns a `Char` value
    b) has zero arguments
    c) takes a tuple argument and returns the first value
    d) requires that `a` and `b` be of different types
    Answer: c)

## Determine the type

Do your best to determine the most polymorphic type an expression could have in the following exercises.

1. All function applications return a value. Determine the value
returned by these function applications and the type of that
value.
    a) `(* 9) 6`
    Answer: `Num a => a`

    b) `head [(0,"doge"),(1,"kitteh")]`
    Answer: `Num a => (a, [Char])`

    c) `head [(0 :: Integer ,"doge"),(1,"kitteh")]`
    Answer: `(Integer, [Char])`

    d) `if False then True else False`
    Answer: `Bool`

    e) `length [1, 2, 3, 4, 5]`
    Answer: `Int`

    f) `(length [1, 2, 3, 4]) > (length "TACOCAT")`
    Answer: `Bool`

2. Given

    ```haskell
    x = 5
    y = x + 5
    w = y * 10
    ```

    What is the type of `w`?

    Answer: `Num a => a`

3. Given
    ```haskell
    x = 5
    y = x + 5
    z y = y * 10
    ```
    What is the type of `z`?
    Answer: `Num a => a -> a`

4. Given
    ```haskell
    x = 5
    y = x + 5
    f = 4 / y
    ```
    What is the type of `f`?
    Answer: `Fractional a => a`

5. Given
    ```haskell
    x = "Julie"
    y = " <3 "
    z = "Haskell"
    f = x ++ y ++ z
    ```
    What is the type of f?
    Answer: `[Char]`

## Does it compile?

For each set of expressions, figure out which expression, if any, causes the compiler to squawk at you (n.b. we do not mean literal squawking) and why. Fix it if you can.

1.
```haskell
bigNum = (^) 5 $ 10
wahoo = bigNum $ 10
```
Compiler error at second line. `bigNum` is not a function. Possible fix:
```haskell
bigNum = (^) 5
wahoo = bigNum $ 10
```

2.
```haskell
x = print
y = print "woohoo!"
z = x "hello world"
```
Compiles without error.

3.
```haskell
a = (+)
b = 5
c = b 10
d = c 200
```
Compiler error at line 3 because `b` is not a function. Possible fix:
```haskell
a = 5
b = (+)
c = b 10
d = c 200
```

4.
```haskell
a = 12 + b
b = 10000 * c
```
Compiler error at line 2, because `c` is not defined.
Possible fix:
```haskell
a = 12 + b
b = 10000 * a
```
This will compile, but will cause infinite loop if we try to evaluate either `a` or `b`.

## Type variable or specific type constructor?

You will be shown a type declaration, and you should categorize each type. The choices are a fully polymorphic type variable, constrained polymorphic type variable, or concrete type constructor.


```haskell
f :: Num a => a -> b -> Int -> Int
--           [0]  [1]   [2]    [3]
```
Here, the answer would be: constrained polymorphic (`Num`) ([0]),
fully polymorphic ([1]), and concrete ([2] and [3]).

2. Categorize each component of the type signature as described in the previous example.
    ```haskell
    f :: zed -> Zed -> Blah
    --   [0]    [1]    [2]
    ```
    [0] fully
    [1] concrete
    [3] concrete

3. Categorize each component of the type signature
    ```haskell
    f :: Enum b => a -> b -> C
                  [0]  [1]  [2]
    ```

    [0] fully
    [1] constrained
    [2] concrete

4. Categorize each component of the type signature
    ```haskell
    f :: f -> g -> C
        [0]  [1]  [2]
    ```
    [0] fully
    [1] fully
    [2] concrete

## Write a type signature

1.
```haskell
functionH :: [a] -> a
functionH (x:_) = x
```

2.
```haskell
functionC :: Ord a => a -> a -> Bool
functionC x y =
    if (x > y) then True else False
```

3.
```haskell
functionS :: (a, b) -> b
functionS (x, y) = y
```

## Given a type, write the function
1. There is only one function definition that typechecks and doesn’t go into an infinite loop when you run it.

    Answer:
    ```haskell
    i :: a -> a
    i a = a
    ```

2. There  is only one version that works.

    Answer:
    ```haskell
    c :: a -> b -> a
    c a b = a
    ```
3. Given alpha equivalence are `c''` and `c` (see above) the same thing?
    ```haskell
    c'' :: b -> a -> b
    ```
    Answer: Yes

4. Only one version that works.
    ```haskell
    c' :: a -> b -> b
    c' a b = b
    ```

5. There are multiple possibilities, at least two of which you’ve seen in previous chapters.
    ```haskell
    r :: [a] -> [a]
    r = tail
    ```

6. Only one version that will typecheck.
    ```haskell
    co :: (b -> c) -> (a -> b) -> a -> c
    co bc ab a = bc (ab a)
    ```

7. One version will typecheck.
```haskell
a :: (a -> c) -> a -> a
a _ x = x
```

8. One version will typecheck.
```haskell
a' :: (a -> b) -> a -> b
a' ab a = ab a
```

## Fix it
1.
```haskell
module sing where

fstString :: [Char] ++ [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> Char
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x or sndString y
where x = "Singin"
      x = "Somewhere"
```
Fixed:
```haskell
module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if (x > y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"
```

3.
```haskell
module Arith3Broken where

main :: IO ()
Main = do
    print 1 + 2
    putStrLn 10
    print (negate -1)
    print ((+) 0 blah)
    where blah = negate 1
```

Fixed:
```haskell
module Arith3Broken where

main :: IO ()
main = do
    print (1 + 2)
    putStrLn "10"
    print (negate (-1))
    print ((+) 0 blah)
    where blah = negate 1
```

## Type-Kwon-Do

1.
```haskell
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g (f x)
```

2.
```haskell
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)
```

3.
```haskell
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)
```


4.
```haskell

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xy ywz x = w
    where
        y = xy x
        (w, z) = ywz y
```