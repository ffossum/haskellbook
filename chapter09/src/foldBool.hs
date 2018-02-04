module FoldBool where

import Data.Bool (bool)

original :: [Integer]
original = map (\x -> if x == 3 then (-x) else (x)) [1..10]

rewritten :: [Integer]
rewritten = map (\x -> bool x (-x) (x == 3)) [1..10]
