# Scope

1. These lines of code are from a REPL session. Is `y` in scope for `z`?
    ```haskell
    Prelude> let x = 5
    Prelude> let y = 7
    Prelude> let z = x * y
    ```
    Answer: Yes.

2. These lines of code are from a REPL session. Is `h` in scope for `g`? Go with your gut here.
    ```haskell
    Prelude> let f = 3
    Prelude> let g = 6 * f + h
    ```
    Answer: No. `h` is not in scope.

3. This code sample is from a source file. Is everything we need to execute area in scope?
    ```haskell
    area d = pi * (r * r)
    r = d / 2
    ```
    Answer: No. `d` is not in scope for `r`.

4. This code is also from a source file. Now are `r` and `d` in scope for area?

    ```haskell
    area d = pi * (r * r)
        where r = d / 2
    ```
    Answer: Yes.