module WarmingUp where

import           Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  capped <- cap
  reversed <- rev
  return (capped, reversed)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind =
  cap >>= (\capped -> rev >>= (\reversed -> return (capped, reversed)))
