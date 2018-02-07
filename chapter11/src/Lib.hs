{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

import Data.Int

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data DogueDeBordeaux doge =
    DogueDeBordeaux doge

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar



data Size = Size Integer deriving (Eq, Show)

doge = Plane PapuAir (Size 100)

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False


class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Integer where
    tooMany n = n > 420

instance TooMany Price where
    tooMany (Price n) = n > 4200

type MyInt = Int


instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

newtype IntString = IntString (Int, String) deriving Show
instance TooMany IntString where
    tooMany (IntString (n, _)) = tooMany n

instance TooMany (Int, Int) where
    tooMany (n, m) = tooMany $ n + m

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

-- parentheses due to syntactic
-- collision between (-) minus
-- and the negate function
myNumba = Numba (-128)
