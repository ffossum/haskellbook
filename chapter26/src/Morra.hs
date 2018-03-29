module Morra where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           System.Random

data GameState = GameState
  { playerScore   :: Int
  , computerScore :: Int
  } deriving (Show)

incPlayerScore :: GameState -> GameState
incPlayerScore gs = gs {playerScore = (playerScore gs + 1)}

incComputerScore :: GameState -> GameState
incComputerScore gs = gs {computerScore = (computerScore gs + 1)}

parseOneOrTwo :: String -> Maybe Int
parseOneOrTwo str =
  case reads str of
    [(v@1, "")] -> Just v
    [(v@2, "")] -> Just v
    _           -> Nothing

getPlayerChoice :: IO Int
getPlayerChoice = do
  l <- putStr "Enter 1 or 2: " >> getLine
  case parseOneOrTwo l of
    (Just choice) -> return choice
    (Nothing) -> do
      putStrLn "Invalid input!"
      getPlayerChoice

getComputerChoice :: IO Int
getComputerChoice = randomRIO (1, 2)

morra :: StateT GameState IO ()
morra = do
  playerChoice <- liftIO $ getPlayerChoice
  computerChoice <- liftIO $ getComputerChoice
  liftIO $ putStr "Computer chooses " >> print computerChoice
  let playerWon = odd (playerChoice + computerChoice)
  if playerWon
    then do
      modify incPlayerScore
      liftIO $ putStrLn "Player wins!"
    else do
      modify incComputerScore
      liftIO $ putStrLn "Computer wins."

main :: IO ()
main = do
  print "Player is odds, computer is evens."
  runStateT (forever morra) (GameState 0 0)
  return ()
