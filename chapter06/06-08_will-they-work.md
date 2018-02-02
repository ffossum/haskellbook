#Exercises: Will they work
Next, take a look at the following code examples and try to decide if
they will work, what result they will return if they do, and why or
why not (be sure, as always, to test them in your REPL once you have
decided on your answer):

1.
```haskell
max (length [1, 2, 3])
    (length [8, 9, 10, 11, 12])
```
Answer:
Yes, result is `5 :: Int`.

2.
```haskell
compare (3 * 4) (3 * 5)
```
Answer:
Yes, result is `LT`.

3.
```haskell
compare "Julie" True
```
Answer: Does not work. Can't compare `[Char]` and `Bool`.
4.
```haskell
(5 + 3) > (3 + 6)
```
Answer: Yes, result is `False`.
