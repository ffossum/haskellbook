module FizzBuzz where

import           Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1 .. 100]

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

stateMain :: IO ()
stateMain = mapM_ putStrLn $ reverse $ fizzbuzzList [1 .. 100]

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = execState (mapM_ addResult [to,(to - 1) .. from]) []

stateMain2 :: IO ()
stateMain2 = mapM_ putStrLn $ fizzbuzzFromTo 1 100
