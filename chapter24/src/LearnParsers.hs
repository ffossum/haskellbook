module LearnParsers where

import           Control.Applicative
import           Test.Hspec
import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1' <* eof

-- read a single character '1', then die
one' = one >> stop

-- equivalent to char '1' >> stop
-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2' <* eof

-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

-- Parsing Practice
-- [1]
oneEof :: Parser Char
oneEof = char '1' <* eof

oneTwoEof :: Parser Char
oneTwoEof = char '1' >> char '2' <* eof

-- [2]
oneStr :: Parser String
oneStr = (string' "123" <|> string' "12" <|> string' "1") <* eof

-- [3]
string' :: String -> Parser String
string' = try . traverse char

-- Exercise: Unit of Success
myParse :: Parser Integer
myParse = integer <* eof
