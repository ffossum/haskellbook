{-# LANGUAGE InstanceSigs #-}

module Lib
  ( someFunc
  ) where

import           Reader

someFunc :: IO ()
someFunc = putStrLn "someFunc"

boop :: Num a => a -> a
boop = (* 2)

doop :: Num a => a -> a
doop = (+ 10)

bip :: Num a => a -> a
bip = boop . doop

ask :: Reader a a
ask = Reader id

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

person =
  Person
  { humanName = (HumanName "Fredrik")
  , dogName = (DogName "Fido")
  , address = (Address "Oslo")
  }

-- with Reader
getDogR :: Person -> Dog
getDogR = runReader $ (pure Dog) <*> (Reader dogName) <*> (Reader address)

getDogRM :: Person -> Dog
getDogRM =
  runReader $ do
    name <- Reader dogName
    addy <- Reader address
    return (Dog name addy)
