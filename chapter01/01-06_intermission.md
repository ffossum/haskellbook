# Intermission

1. `λxy.xz`

    a) `λxz.xz`

    b) `λmn.mz`
    
    c) `λz.(λx.xz)`

    Answer: b)
    
    First variable in body must be bound to first parameter.
    Second variable in body must be a free variable `z`.

    In a) and c) `z` is not a free variable.


2. `λxy.xxy`

    a) `λmn.mnp`

    b) `λx.(λy.xy)`

    c) `λa.(λb.aab)`

    Answer: c)

    λxy.xxy can be rewritten as `λx.(λy.xxy)` which is equivalent to `λa.(λb.aab)`.


3. `λxyz.zx`

    a) `λx.(λy.(λz.z))`

    b) `λtos.st`

    c) `λmnp.mn`

    Answer: b)
