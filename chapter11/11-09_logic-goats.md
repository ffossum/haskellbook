# Exercises: Logic Goats

1. Reusing the TooMany typeclass, write an instance of the typeclass
for the type (Int, String). This will require adding a language pragma named FlexibleInstances if you do not use a newtype — GHC will tell you what to do.

Answer:
```haskell

{-# LANGUAGE FlexibleInstances #-}
instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n
-- or
newtype IntString = IntString (Int, String) deriving Show
instance TooMany IntString where
    tooMany (IntString (n, _)) = tooMany n

```

2. Make another TooMany instance for (Int, Int). Sum the values together under the assumption this is a count of goats from two fields.
    Answer:
    ```haskell
    instance TooMany (Int, Int) where
        tooMany (n, m) = tooMany $ n + m
    ```

