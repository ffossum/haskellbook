module VigenereIO where

import           Cipher
import           Control.Monad
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error

handleIOError :: IOError -> IO ()
handleIOError err
  | isEOFError err = exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    keyword:"-d":_ -> do
      catchIOError
        (forever $
         forM_
           keyword
           (\keyChar -> do
              c <- hGetChar stdin
              hPutChar stdout $ unvigenereChar keyChar c))
        handleIOError
    keyword:"-e":_ -> do
      catchIOError
        (forever $
         forM_
           keyword
           (\keyChar -> do
              c <- hGetChar stdin
              hPutChar stdout $ vigenereChar keyChar c))
        handleIOError
    _ -> do
      putStrLn "Illegal arguments"
      exitWith (ExitFailure 1)
