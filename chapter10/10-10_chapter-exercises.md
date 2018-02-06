# Chapter Exercises

## Warm-up and review

For the following set of exercises, you are not expected to use folds. These are intended to review material from previous chapters. Feel free to use any syntax or structure from previous chapters that seems appropriate.

1. Given the following sets of consonants and vowels:
```haskell
stops = "pbtdkg"
vowels = "aeiou"
```

a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations.

These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.

Answer:
```haskell
combinations = [ [a,b,c] | a <- stops, b <- vowels, c <- stops ]
```

b) Modify that function so that it only returns the combina-
tions that begin with a p.

Answer:
```haskell
combinations = [ ['p',b,c] | b <- vowels, c <- stops ]
```

c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.

```haskell
sentences nouns verbs = [ a ++ " " ++ b ++ " " ++ c | a <- nouns, b <- verbs, c <- nouns ]
```

2. What does the following mystery function do? What is its type? Try to get a good sense of what it does before you test it in the REPL to verify it.

```haskell
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))
```

Answer: It calculates the average word length in a string. Rounded down to an integer.

3. We’d really like the answer to be more precise. Can you rewrite that using fractional division?

```haskell
seekritFunc x =
    (fromIntegral totalWordLength) / (fromIntegral numberOfWords)
    where
        ws = words x
        totalWordLength = sum (map length ws)
        numberOfWords = length ws
```

## Rewriting functions using folds

Answer: [rewritingUsingFolds.hs](./src/rewritingUsingFolds.hs)
