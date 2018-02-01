module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Trivial =
    Trivial

instance Eq Trivial where
    Trivial == Trivial = True

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
-- day of week and numerical day of month
data Date =
    Date DayOfWeek Int

data TisAnInteger =
    TisAn Integer


instance Eq TisAnInteger where
    TisAn a == TisAn b = a == b

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    Two a1 a2 == Two b1 b2 = a1 == b1 && a2 == b2
    -- Two a1 a2 == Two b1 b2 = (a1, a2) == (b1, b2)

data StringOrInt =
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    TisAnInt a == TisAnInt b = a == b
    TisAString a == TisAString b = a == b
    _ == _ = False



data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    Pair a1 a2 == Pair a3 a4 = a1 == a3 && a2 == a4

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    Tuple a1 b1 == Tuple a2 b2 = a1 == a2 && b1 == b2

data Which a =
      ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
    ThisOne a1 == ThisOne a2 = a1 == a2
    ThatOne a1 == ThatOne a2 = a1 == a2
    _ == _ = False


data EitherOr a b =
      Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    Hello a1 == Hello a2 = a1 == a2
    Goodbye b1 == Goodbye b2 = b1 == b2
    _ == _ = False
