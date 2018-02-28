module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter isGameWord aw)
  where
    isGameWord w =
      let l = length (w :: String)
      in l >= minWordLength && l < maxWordLength && lettersOnly w

lettersOnly :: String -> Bool
lettersOnly = (all (`elem` letters)) . (map toLower)
  where
    letters = ['a' .. 'z']

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle
  { word    :: String
  , guessed :: [Char]
  } deriving (Eq)

instance Show Puzzle where
  show p@(Puzzle _ gs) =
    (intersperse ' ' $ fmap renderPuzzleChar ds) ++ " Guessed so far: " ++ gs
    where
      ds = discovered p

discovered :: Puzzle -> [Maybe Char]
discovered (Puzzle w gs) = map f w
  where
    f a
      | a `elem` gs = Just a
      | otherwise = Nothing

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _) = (`elem` w)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ g) = (`elem` g)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = maybe '_' id

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@Puzzle {guessed = g} c = p {guessed = c : g}

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the\
        \ word, filling in the word\
        \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
        \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver p =
  if (wrongGuesses p) > 5
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ (word p)
      exitSuccess
    else return ()

wrongGuesses :: Puzzle -> Int
wrongGuesses puzzle =
  length $ filter (not . (charInWord puzzle)) (guessed puzzle)

gameWin :: Puzzle -> IO ()
gameWin (Puzzle w gs) =
  if all (`elem` gs) w
    then do
      putStrLn "You win!"
      putStrLn $ "The word was: " ++ w
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  w <- randomWord'
  let puzzle = freshPuzzle (fmap toLower w)
  runGame puzzle
