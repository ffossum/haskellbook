module WhatHappens where

import           Control.Concurrent
import           Debug.Trace

blah :: IO String
blah = return "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

main :: IO ()
main = do
  x <- blah'
  y <- blah'
  putStrLn x
  putStrLn y
  x' <- woot
  y' <- woot
  putStrLn x'
  putStrLn y'
  return ()
