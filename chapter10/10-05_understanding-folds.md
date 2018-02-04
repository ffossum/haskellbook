# Exercises: Understanding Folds

1. `foldr (*) 1 [1..5]`
    will return the same result as which of the following:

    a) `flip (*) 1 [1..5]`
    b) `foldl (flip (*)) 1 [1..5]`
    c) `foldl (*) 1 [1..5]`

    Answer: b) and c). `foldl`/`foldr` and `flip`/no `flip` doesn't matter because `(*)` is associative.


2. Write out the evaluation steps for
`foldl (flip (*)) 1 [1..3]`
    Answer:
    ```haskell
    f = flip (*)

    (((1 `f` 1) `f` 2) `f` 3)
     ((1 `f` 2) `f` 3)
      (2 `f` 3)
       1
    ```
3. One difference between `foldr` and `foldl` is:
a) `foldr`, but not `foldl`, traverses the spine of a list from right to left
b) `foldr`, but not `foldl`, always forces the rest of the fold
**c) `foldr`, but not `foldl`, associates to the right**
d) `foldr`, but not `foldl`, is recursive

4. Folds are catamorphisms, which means they are generally used to
**a) reduce structure**
b) expand structure
c) render you catatonic
d) generate infinite data structures

5. The following are simple folds very similar to what you’ve already seen, but each has at least one error. Please fix them and test in your REPL:

a) `foldr (++) ["woot", "WOOT", "woot"]`
Answer: `foldr (++) "" ["woot", "WOOT", "woot"]`

b) `foldr max [] "fear is the little death"`
Answer: `foldr max minBound "fear is the little death"`

c) `foldr and True [False, True]`
Answer: `foldr (&&) True [False, True]`

d) This one is more subtle than the previous. Can it ever return a different answer?
`foldr (||) True [False, True]`
Answer:
`foldr (||) False [False, True]`

e) `foldl ((++) . show) "" [1..5]`
Answer:
`foldl (flip ((++) . show)) "" [1..5]`
or
`foldr ((++) . show) "" [1..5]`

f) `foldr const 'a' [1..5]`
Answer: `foldr (flip const) 'a' [1..5]`

g) `foldr const 0 "tacos"`
Answer: `foldr (flip const) 0 "tacos"`

h) `foldl (flip const) 0 "burritos"`
Answer: `foldl const 0 "burritos"`

i) `foldl (flip const) 'z' [1..5]`
Answer: `foldl const 'z' [1..5]`

