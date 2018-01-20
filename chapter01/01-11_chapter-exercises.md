# Chapter Exercises

## Combinators
Determine if each of the following are combinators or not.

Combinator is a function with no free variables.

1. `λx.xxx`
Yes

2. `λxy.zx`
No, because z is a free variable.

3. `λxyz.xy(zx)`
Yes

4. `λxyz.xy(zxy)`
Yes

5. `λxy.xy(zxy)`
No, because z is a free variable.


## Normal form or diverge?
Determine if each of the following can be reduced to a normal form or if they diverge.

1. `λx.xxx`
Normal Form

2. `(λz.zz)(λy.yy)`
Diverges.

3. `(λx.xxx)z`
Reduces to zzz


## Beta reduce 
Evaluate (that is, beta reduce) each of the following
expressions to normal form.

1. `(λabc.cba)zz(λwv.w)`

    `(λa.λb.λc.cba)zz(λwλv.w)`
    
    `(λc.czz)(λwλv.w)`
    
    `(λwλv.w)zz`
    
    `(λv.z)z`
    
    `z`
    
2. `(λx.λy.xyy)(λa.a)b`

    `(λy.(λa.a)yy)b`

    `(λa.a)bb`

    `bb`

3. `(λy.y)(λx.xx)(λz.zq)`

    `(λx.xx)(λz.zq)` 

    `(λz.zq)(λz.zq)`

    `(λz.zq)q`

    `qq`

4. `(λz.z)(λz.zz)(λz.zy)` Hint: alpha equivalence.
    
    Equivalent to 3.
    
    `yy`

5. `(λx.λy.xyy)(λy.y)y`

    Same as 2. except last free variable `b` is named `y` instead.
    
    `yy`

6. `(λa.aa)(λb.ba)c`

    `(λb.ba)(λb.ba)c`

    `(λb.ba)ac`

    `aac`

7. `(λxyz.xz(yz))(λx.z)(λx.a)`

    `(λx.λy.λz.xz(yz))(λx.z)(λx.a)`

    `(λy.λz.(λx.z')z(yz))(λx.a)`

    `(λz.(λx.z')z(λx.a)z)`

    `(λz.z'a)`
