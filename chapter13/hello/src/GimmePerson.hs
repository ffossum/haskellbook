module GimmePerson where

import           Data.Maybe

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter name: "
  name <- getLine
  putStrLn "Enter age: "
  age <- getAge
  let person = mkPerson name age
  case person of
    (Right p)  -> putStrLn $ "Yay! Successfully got a person: " ++ show p
    (Left err) -> putStrLn $ "Error: " ++ show err

getAge :: IO Integer
getAge = do
  str <- getLine
  age <- return $ parseInteger str
  case age of
    (Just a) -> return a
    Nothing -> do
      putStrLn "Not a valid number! Try again."
      getAge

parseInteger :: String -> Maybe Integer
parseInteger str =
  case (reads :: ReadS Integer) str of
    []        -> Nothing
    [(a, "")] -> Just a
    _         -> Nothing
