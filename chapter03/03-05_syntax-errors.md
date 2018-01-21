# Syntax Errors

Fix the syntax errors.

1.
```haskell
++ [1, 2, 3] [4, 5, 6]
```
Fixed:
```haskell
(++) [1, 2, 3] [4, 5, 6]
```

or

```haskell
[1, 2, 3] ++ [4, 5, 6]
```

2.
```haskell
'<3' ++ ' Haskell'
```
Fixed:
```haskell
"<3" ++ " Haskell"
```

3.
```haskell
concat ["<3", " Haskell"]
```
No error.