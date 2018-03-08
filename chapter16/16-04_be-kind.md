# Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. What’s the kind of `a`?
    ```haskell
    a -> a
    ```
    Answer: `*`

2. What are the kinds of `b` and `T` ? (The `T` is capitalized on purpose!)
    ```haskell
    a -> b a -> T (b a)
    ```
    Answer:
    ```haskell
    a :: *
    b :: * -> *
    T :: * -> *
    ```

3. What’s the kind of `c`?
    ```haskell
    c a b -> c b a
    ```
    Answer: `* -> * -> *`