module Lib where

import Data.List (sort)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Trivial =
    Trivial

instance Eq Trivial where
    Trivial == Trivial = True

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Eq, Ord, Show)

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

a = max (length [1, 2, 3])
        (length [8, 9, 10, 11, 12])

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                then Blah
                else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
    Rocks String deriving (Eq, Show)

data Yeah =
    Yeah Bool deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

-- phew = Papu "chases" True
truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

myX = 1 :: Int

mySort :: [Char] -> [Char]
mySort = sort

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith f i a = (fromInteger i) + f a