```haskell
data Quantum =
      Yes
    | No
    | Both
    deriving (Eq, Show)
```

## Exponentiation in what order?

Consider the following function:
```haskell
convert :: Quantum -> Bool
convert = undefined
```

According to the equality of a -> b and b^a^ there should be 2^3^ or 8
implementations of this function. Does this hold? Write it out and
prove it for yourself.
Answer:
```haskell
-- 1.
convert Yes  = False
convert No   = False
convert Both = False

-- 2.
convert Yes  = True
convert No   = False
convert Both = False

-- 3.
convert Yes  = False
convert No   = True
convert Both = False

-- 4.
convert Yes  = True
convert No   = True
convert Both = False

-- 5.
convert Yes  = False
convert No   = False
convert Both = True

-- 6.
convert Yes  = True
convert No   = False
convert Both = True

-- 7.
convert Yes  = False
convert No   = True
convert Both = True

-- 8.
convert Yes  = True
convert No   = True
convert Both = True

```


# Exercises: The Quad

Determine how many unique inhabitants each type has.
Suggestion: do the arithmetic unless you want to verify. Writing them out gets tedious quickly.

1.
```haskell
data Quad =
      One
    | Two
    | Three
    | Four
    deriving (Eq, Show)

-- how many different forms can this take?
eQuad :: Either Quad Quad
eQuad = ???
```
Answer: 4 + 4 = 8

2. ```prodQuad :: (Quad, Quad)```
Answer: 4 * 4 = 16

3. ```funcQuad :: Quad -> Quad```
Answer: 4^4^ = 256

4. ```prodTBool :: (Bool, Bool, Bool)```
Answer: 2 * 2 * 2 = 8

5. ```gTwo :: Bool -> Bool -> Bool```
Answer: 2 ^ 2 ^ 2 = 16

6. Hint: 5 digit number
    ```fTwo :: Bool -> Quad -> Quad```
    Answer: (4^4^)^2^ = 65536