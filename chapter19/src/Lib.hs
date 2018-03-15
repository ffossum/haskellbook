module Lib where

import           Control.Applicative
import qualified Data.Map            as M
import           Data.Semigroup

f :: M.Map Char Int
f = M.fromList [('a', 1)]

g :: M.Map Char Int
g = M.fromList [('b', 2)]

h :: M.Map Char Int
h = f <> g

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myReplicateM :: Applicative m => Int -> m a -> m [a]
myReplicateM n ma = foldr (liftA2 (:)) (pure []) mas
  where
    mas = replicate n ma
