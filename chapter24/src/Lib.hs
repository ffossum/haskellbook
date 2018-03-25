module Lib
  ( someFunc
  ) where

import           Control.Monad.Trans.State

someFunc :: IO ()
someFunc = putStrLn "someFunc"

x =
  runState $ do
    put 2
    return 9001
