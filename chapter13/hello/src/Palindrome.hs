module Palindrome where

import           Data.Char
import           Control.Monad
import           System.Exit

isPalindrome :: String -> Bool
isPalindrome str = preparedStr == reverse preparedStr
  where
    preparedStr = prepare str
    prepare = (filter isAlpha) . (map toLower)

palindrome :: IO ()
palindrome =
  forever $ do
    line1 <- getLine
    case (isPalindrome line1) of
      True -> putStrLn "It's a palindrome!"
      False -> do
        putStrLn "Nope!"
        exitSuccess
