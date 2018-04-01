module Lib
  ( someFunc
  ) where

import           Criterion.Main
import qualified Data.Sequence  as S

lists :: [Int]
lists = [1 .. 100000]

seqs :: S.Seq Int
seqs = S.fromList [1 .. 100000]

main :: IO ()
main =
  defaultMain
    [ bench "indexing list" $ whnf (\xs -> xs !! 9001) lists
    , bench "indexing sequence" $ whnf (flip S.index 9001) seqs
    ]

someFunc :: IO ()
someFunc = putStrLn "someFunc"

s :: String
s = "\12371\12435\12395\12385\12399\12289\20803\27671\12391\12377\12363\65311"
