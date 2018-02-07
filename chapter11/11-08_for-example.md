# Exercises: For Example
```haskell
data Example = MakeExample deriving Show
```

1. You can query the type of a value in GHCi with the :type com-
mand, also abbreviated :t.

What is the type of data constructor MakeExample? What happens
when you request the type of Example?

Answer: The type of `MakeExample` is `Example`.

Requesting the type of `Example` results in an error, because it is not a value, but a type. You can't get the type of a type.

2. What if you try :info on Example in GHCi? Can you determine what typeclass instances are defined for the Example type using :info in GHCi?

Answer:
`Example` only has an instance of one typeclass: `Show`, which was derived.

3. Try making a new datatype like Example but with a single type argument added to MakeExample, such as Int. What has changed when you query MakeExample with :type in GHCi?

Answer:
```haskell
data Example2 = MakeExample2 Int deriving Show
```
`MakeExample` now has type `Int -> Example2`.