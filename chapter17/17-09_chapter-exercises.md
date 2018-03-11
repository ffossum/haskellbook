# Chapter exercises

Given a type that has an instance of `Applicative`, specialize the types
of the methods. Test your specialization in the REPL. One way to
do this is to bind aliases of the typeclass methods to more concrete
types that have the type we told you to fill in.

```haskell
-- Type
?

-- Methods
pure :: a -> ? a
(<*>) :: ? (a -> b) -> ? a -> ? b
```

1.
```haskell
-- Type
[]

-- Methods
pure :: a -> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]
```
2.
```haskell
-- Type
IO

-- Methods
pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b
```
3.
```haskell
-- Type
(,) e

-- Methods
pure :: a -> (e, a)
(<*>) :: (e, (a -> b)) -> (e, a) -> (e, b)
```
4.
```haskell
-- Type
(->) e

-- Methods
pure :: a -> (e -> a)
(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
```

Write instances for the following datatypes. Confused? Write
out what the type should be. Use the `checkers` library to validate the
instances.

[ApplicativeInstances.hs](./src/ApplicativeInstances.hs)