1. Given a datatype
```haskell
data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)
```

What is the cardinality of this datatype? Hint: We already know `Bool`’s cardinality. Show your work as demonstrated earlier.

Answer:

```
Big Bool | Small Bool
2        | 2
2 + 2
4
```

2. Given a datatype
```haskell
-- bring Int8 in scope
import Data.Int
data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- parentheses due to syntactic
-- collision between (-) minus
-- and the negate function
let myNumba = Numba (-128)
````

What is the cardinality of `NumberOrBool`?
Answer:
```
Int8 | Bool
256  + 2
258
```
What happens if you try to create a Numba with a numeric literal larger than 127? And with a numeric literal smaller than (-128)?
Answer: You get a warning, and the value overflows, so you get a different value than (probably) intended.

If you choose (-128) for a value precisely, you’ll notice you get a spurious warning: