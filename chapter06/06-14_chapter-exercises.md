# Chapter Exercises

## Multiple choice

(Correct answer highlighted)

1. The `Eq` class
a) includes all types in Haskell
b) is the same as the `Ord` class
**c) makes equality tests possible**
d) only includes numeric types

2. The typeclass `Ord`
a) allows any two values to be compared
**b) is a subclass of `Eq`**
c) is a superclass of `Eq`
d) has no instance for Bool

3. Suppose the typeclass `Ord` has an operator `>`. What is the type of `>`?
**a) `Ord a => a -> a -> Bool`**
b) `Ord a => Int -> Bool`
c) `Ord a => a -> Char`
d) `Ord a => Char -> [Char]`

4. In `x = divMod 16 12`
a) the type of `x` is Integer
b) the value of `x` is undecidable
**c) the type of `x` is a tuple**
d) `x` is equal to `12 / 16`

5. The typeclass `Integral` includes
**a) `Int` and `Integer` numbers**
b) integral, real, and fractional numbers
c) Schrodinger’s cat
d) only positive numbers

## Does it typecheck

1. Does the following code typecheck? If not, why not?
```haskell
data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
```

Answer: Does not typecheck, because `Person` does not have an instance of `Show`.

2. Does the following typecheck? If not, why not?
```haskell

data Mood = Blah
          | Woot deriving Show

settleDown x = if x == Woot
                then Blah
                else x
```
Answer: Does not typecheck, because `Mood` does not have an instance of `Eq`.

Fixed:
```haskell

data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                then Blah
                else x
```
3. If you were able to get `settleDown` to typecheck:

a) What values are acceptable inputs to that function?
Answer: Any `Mood` is an acceptable input. This means either `Blah` or `Woot`.
b) What will happen if you try to run `settleDown 9`? Why?
Answer: An error. Because 9 is not a `Mood`.
c) What will happen if you try to run `Blah > Woot`? Why?
Answer: An error, because `Mood` does not have an instance of `Ord`.

4. Does the following typecheck? If not, why not?
```haskell
type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
```

Answer: Yes, it typechecks. `s1` is missing the last parameter to create a complete `Sentence`, but because of currying this is no problem. `s1` is a function of type `Object -> Sentence`.

## Given a datatype declaration, what can we do?
Given the following datatype definitions:

```haskell
data Rocks =
    Rocks String deriving (Eq, Show)

data Yeah =
    Yeah Bool deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)
```

Which of the following will typecheck? For the ones that don’t typecheck, why don’t they?

1.
```haskell
phew = Papu "chases" True
```
This does not typecheck. The parameters must be wrapped in the correct data types.

2.
```haskell
truth = Papu (Rocks "chomskydoz") (Yeah True)
```
This typechecks.

3.
```haskell
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
```
This typechecks.

4.
```haskell
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
```
This does not typecheck, because `Papu` does not have an instance of `Ord`.

##Match the types

We’re going to give you two types and their implementations. Then we’re going to ask you if you can substitute the second type for the first. You can test this by typing the first declaration and its type into a file and editing in the new one, loading to see if it fails. Don’t guess, test all your answers!

1. For the following definition.
a)
```haskell
i :: Num a => a
i = 1
```

b) Try replacing the type signature with the following:
```haskell
i :: a
```

This does not work. `Num a => a` is already the least specific constraint a number can have.

2.

a)
```haskell
f :: Float
f = 1.0
```

b)
```haskell
f :: Num a => a
```
This does not work. `1.0` receives the type `Fractional a => a`, and we can't change it to the less specific `Num a => a`.

3.
a)
```haskell
f :: Float
f = 1.0
```
b)
```haskell
f :: Fractional a => a
```
This works.

4. Hint for the following: type :info RealFrac in your REPL.
a)
```haskell
f :: Float
f = 1.0
```
b)
```haskell
f :: RealFrac a => a
```

This works. `RealFrac` is a subclass of `Fractional`

5.
a)
```haskell
freud :: a -> a
freud x = x
```
b)
```haskell
freud :: Ord a => a -> a
```

This works.

6.
a)
```haskell
freud' :: a -> a
freud' x = x
```
b)
```haskell
freud' :: Int -> Int
```

This works.

7.
a)
```haskell
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX
```
b)
```haskell
sigmund :: a -> a
```

This does not work. With the signature `a -> a`, `sigmund` must return an `a`, not an `Int`.

8.
a)
```haskell
myX = 1 :: Int

sigmund' :: Int -> Int
sigmund' x = myX
```
b)
```haskell
sigmund' :: Num a => a -> a
```

Still does not work. `sigmund'` must return an `Num a => a`, not the more specialized `Int`.

9.
a) You’ll need to import `sort` from `Data.List`.
```haskell
jung :: Ord a => [a] -> a
jung xs = head (sort xs)
```

b)
```haskell
jung :: [Int] -> Int
```

This works. `Int` has an instance of `Ord`.

10.
a)
```haskell
young :: [Char] -> Char
young xs = head (sort xs)
```

b)
```haskell
young :: Ord a => [a] -> a
```

This works. We're back to what we started with in 9.

11.

a)
```haskell
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
```

b)
```haskell
signifier :: Ord a => [a] -> a
```

This does not work. We can't pass `Ord a => [a]` to a function that requires the more specialized `[Char]`.

## Type-Kwon-Do Two: Electric Typealoo

1.
```haskell
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b
```
2.
```haskell
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith f i a = (fromInteger i) + f a
```
