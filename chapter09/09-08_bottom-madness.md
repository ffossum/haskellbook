# Exercises: Bottom Madness

##Will it blow up?

Will the following expressions return a value or be ⊥?

1. `[x^y | x <- [1..5], y <- [2, undefined]]`
    Answer: Blows up.

2. `take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]`
    Answer: `[1]`

3. `sum [1, undefined, 3]`
    Answer: Blows up.

4. `length [1, 2, undefined]`
    Answer: `3`

5. `length $ [1, 2, 3] ++ undefined`
    Answer: Blows up.

6. `take 1 $ filter even [1, 2, 3, undefined]`
    Answer: `[2]`

7. `take 1 $ filter even [1, 3, undefined]`
    Answer: Blows up.

8. `take 1 $ filter odd [1, 3, undefined]`
    Answer: `[1]`

9. `take 2 $ filter odd [1, 3, undefined]`
    Answer: `[1,3]`

10. `take 3 $ filter odd [1, 3, undefined]`
    Answer: Blows up.

## Intermission: Is it in normal form?

For each expression below, determine whether it’s in:

1. normal form, which implies weak head normal form;
2. weak head normal form only; or,
3. neither.

Remember that an expression cannot be in normal form or weak head normal form if the outermost part of the expression isn’t a data constructor. It can’t be in normal form if any part of the expression is unevaluated.

1. `[1, 2, 3, 4, 5]`
Answer: WHNF and NF

2. `1 : 2 : 3 : 4 : _`
Answer: WHNF

3. `enumFromTo 1 10`
Answer: Neither

4. `length [1, 2, 3, 4, 5]`
Answer: Neither

5. `sum (enumFromTo 1 10)`
Answer: Neither

6. `['a'..'m'] ++ ['n'..'z']`
Answer: Neither

7. `(_, 'b')`
Answer: WHNF

