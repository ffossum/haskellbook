# A Head Code
Rewrite with `where` clauses:

1.
```haskell
let x = 3; y = 1000 in x * 3 + y
```
Answer:
```haskell
mult1 = x * 3 + y
    where
        x = 3
        y = 1000
```

2.
```haskell
let y = 10; x = 10 * 5 + y in x * 5
```
Answer:
```haskell
mult2 = x * 5
    where
        y = 10
        x = 10 * 5 + y
```

3.

```haskell
let x = 7
    y = negate x
    z = y * 10
in z / x + y
```
Answer:
```haskell
mult3 = z / x + y
    where
        x = 7
        y = negate x
        z = y * 10
```
