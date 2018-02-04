# Exercises: Comprehend Thy Lists
```haskell
mySqr = [1,4,9,16,25,36,49,64,81,100]
```

Take a look at the following functions, figure what you think the output lists will be, and then run them in your REPL to verify (note that you will need the `mySqr` list from above in scope to do this):

```haskell
[x | x <- mySqr, rem x 2 == 0]
```
Answer: `[4,16,36,64,100]`

```haskell
[(x, y) | x <- mySqr,
          y <- mySqr,
          x < 50, y > 50]
```
Answer:
```haskell
[(1,64),(1,81),(1,100),
 (4,64),(4,81),(4,100),
 (9,64),(9,81),(9,100),
 (16,64),(16,81),(16,100),
 (25,64),(25,81),(25,100),
 (36,64),(36,81),(36,100),
 (49,64),(49,81),(49,100)]
```

```haskell
take 5 [ (x, y) | x <- mySqr,
                  y <- mySqr,
                  x < 50, y > 50 ]

```
Answer:
```haskell
[(1,64),(1,81),(1,100),
 (4,64),(4,81)]
```

