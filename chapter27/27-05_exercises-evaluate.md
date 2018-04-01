# Exercises: Evaluate

Expand the expression in as much detail as possible. Then, work
outside-in to see what the expression evaluates to.
1. `const 1 undefined`

    Answer:
    ```haskell
    (\a _ -> a) 1 undefined
    1
    ```

2. `const undefined 1`

    Answer:
    ```haskell
    (\a _ -> a) undefined 1
    undefined
    ```
3. `flip const undefined 1`

    Answer:
    ```haskell
    (\f a b -> f b a) (\a _ -> a) undefined 1
    (\a _ -> a) 1 undefined
    1
    ```
4. `flip const 1 undefined`

    Answer:
    ```haskell
    (\f a b -> f b a) (\a _ -> a) 1 undefined
    (\a _ -> a) undefined 1
    undefined
    ```
5. `const undefined undefined`

    Answer:
    ```haskell
    (\a _ -> a) undefined undefined
    undefined
    ```
6. `foldr const 'z' ['a'..'e']`

    Answer:
    ```haskell
    const 'a' (foldr const 'z' ['b'..'e'])
    const 'a' (const 'b' (const 'c' (const 'd' (const 'e' 'z')))
    const 'a' _
    'a'
    ```
7. `foldr (flip const) 'z' ['a'..'e']`

    Answer:
    ```haskell
    const (foldr (flip const) 'z' ['b'..'e']) 'a'
    const (const (const (const (const 'z' 'e') 'd') 'c') 'b') 'a'
    foldr (flip const) 'z' ['b'..'e']
    ...
    foldr (flip const) 'z' ['e']
    const (foldr (flip const) 'z' []) 'e'
    const 'z' 'e'
    'z'
    ```