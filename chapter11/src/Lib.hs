module Lib where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "Hello"

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os   :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [Programmer {os = o, lang = l} | o <- allOperatingSystems, l <- allLanguages]

newtype Name =
  Name String
  deriving (Show)

newtype Acres =
  Acres Int
  deriving (Show)

-- FarmerType is a Sum
data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Eq, Show)

-- Farmer is a plain ole product of
-- Name, Acres, and FarmerType
data Farmer =
  Farmer Name
         Acres
         FarmerType
  deriving (Show)

data FarmerRec = FarmerRec
  { name       :: Name
  , acres      :: Acres
  , farmerType :: FarmerType
  } deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec = (== DairyFarmer) . farmerType


isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf as@(ah:at) (bh:bt) = if ah == bh
  then isSubseqOf at bt
  else isSubseqOf as bt
