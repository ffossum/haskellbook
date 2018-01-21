# Find the Mistakes

The following lines of code may have mistakes — some of them won’t compile! You know what you need to do.

1.

```haskell
not True && true
```
Fixed:
```haskell
not True && True
```

2.

```haskell
not (x = 6)
```
Fixed:
```haskell
not (x == 6)
```

3.

```haskell
(1 * 2) > 5
```
Compiles.

4.


```haskell
[Merry] > [Happy]
```
Fixed:
```haskell
data Mood = Merry | Happy deriving (Eq, Ord)

[Merry] > [Happy]
```

5.

```haskell
[1, 2, 3] ++ "look at me!"
```
Fixed:
```haskell
"123" ++ "look at me!"
```

