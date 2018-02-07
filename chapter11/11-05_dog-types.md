# Exercises: Dog Types

```haskell
data DogueDeBordeaux doge =
    DogueDeBordeaux doge

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)
```

Given the datatypes defined in the above sections,

1. Is `Doggies` a type constructor or a data constructor?
Answer: Type constructor.

2. What is the kind of `Doggies`?
Answer: `* -> *`

3. What is the kind of `Doggies String`?
Answer: `*`

4. What is the type of `Husky 10`?
Answer: `Num a => Doggies a`

5. What is the type of `Husky (10 :: Integer)`?
Answer: `Doggies Integer`.

6. What is the type of `Mastiff "Scooby Doo"`?
Answer: `Doggies [Char]`.

7. Is `DogueDeBordeaux` a type constructor or a data constructor?
Answer: Yes. (both)

8. What is the type of `DogueDeBordeaux`?
Answer: `a -> DogueDeBordeaux a`

9. What is the type of `DogueDeBordeaux "doggie!"`?
Answer: `DogueDeBordeaux [Char]`