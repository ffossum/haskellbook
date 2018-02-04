# Exercises: Thy Fearful Symmetry

1. Using `takeWhile` and `dropWhile`, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into words, as in the following sample:

```
Prelude> myWords "sheryl wants fun"
["wallfish", "wants", "fun"]
```

Answer:
```haskell
myWords :: String -> [String]
myWords s = reverse $ go s []
    where
        go "" res = res
        go s  res = go next (word : res)
            where
                word = takeWhile (/=' ') s
                next = dropWhile (==' ') . dropWhile (/=' ') $ s
```

2. Next, write a function that takes a string and returns a list of strings, using newline separators to break up the string as in the following (your job is to fill in the undefined function):

Answer: [poemLines.hs](./src/poemLines.hs)

3. Now let’s look at what those two functions have in common. Try writing a new function that parameterizes the character you’re breaking the string argument on and rewrite `myWords` and `myLines` using it.

Answer: [mySplit.hs](./src/mySplit.hs)


