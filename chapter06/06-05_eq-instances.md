#Eq Instances

Write the Eq instance for the datatype provided.
1. It’s not a typo, we’re just being cute with the name.
```haskell
data TisAnInteger =
    TisAn Integer
```
Answer:
```haskell
instance Eq TisAnInteger where
    TisAn a == TisAn b = a == b
```

2.
```haskell
data TwoIntegers =
    Two Integer Integer
```
Answer:
```haskell
instance Eq TwoIntegers where
    Two a1 a2 == Two b1 b2 = (a1, a2) == (b1, b2)
```
or
```haskell
instance Eq TwoIntegers where
    Two a1 a2 == Two b1 b2 = a1 == b1 && a2 == b2
```

3.
```haskell
data StringOrInt =
      TisAnInt Int
    | TisAString String
```
Answer:
```haskell
instance Eq StringOrInt where
    TisAnInt a == TisAnInt b = a == b
    TisAString a == TisAString b = a == b
    _ == _ = False
```

4.
```haskell
data Pair a =
    Pair a a
```
Answer:
```haskell
instance Eq a => Eq (Pair a) where
    Pair a1 a2 == Pair a3 a4 = a1 == a3 && a2 == a4
```
5.
```haskell
data Tuple a b =
    Tuple a b
```
Answer:
```haskell
instance (Eq a, Eq b) => Eq (Tuple a b) where
    Tuple a1 b1 == Tuple a2 b2 = a1 == a2 && b1 == b2
```
6.
```haskell
data Which a =
      ThisOne a
    | ThatOne a
```
Answer:
```haskell
instance Eq a => Eq (Which a) where
    ThisOne a1 == ThisOne a2 = a1 == a2
    ThatOne a1 == ThatOne a2 = a1 == a2
    _ == _ = False
```
7.
```haskell
data EitherOr a b =
      Hello a
    | Goodbye b
```
Answer:
```haskell
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    Hello a1 == Hello a2 = a1 == a2
    Goodbye b1 == Goodbye b2 = b1 == b2
    _ == _ = False
```