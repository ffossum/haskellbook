module Phone where

import           Data.Char
import           Data.List
import           Data.Ord

-- fill in the rest.
data DaPhone = DaPhone
  { buttons :: [Button]
  }

data Button = Button
  { symbol  :: Char
  , letters :: [Char]
  }

phone :: DaPhone
phone =
  DaPhone
    [ (Button '1' "1")
    , (Button '2' "abc2")
    , (Button '3' "def3")
    , (Button '4' "ghi4")
    , (Button '5' "jkl5")
    , (Button '6' "mno6")
    , (Button '7' "pqrs7")
    , (Button '8' "tuv8")
    , (Button '9' "wxyz9")
    , (Button '*' "^")
    , (Button '0' " +_0")
    , (Button '#' ",-#")
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps daPhone c
  | isUpper c = (reverseTaps daPhone '^') ++ (reverseTaps daPhone $ toLower c)
  | otherwise = maybe [] id result
  where
    result = do
      button <- find (containsLetter c) (buttons daPhone)
      letterIndex <- findIndex (== c) (letters button)
      presses <- return $ letterIndex + 1
      return [(symbol button, presses)]

containsLetter :: Char -> Button -> Bool
containsLetter c (Button _ ls) = elem c ls

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daPhone = concat . map (reverseTaps daPhone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . (map snd)

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular

occurences :: Eq a => [a] -> a -> Int
occurences as a = length $ filter (== a) as

mostPopular :: Eq a => [a] -> a
mostPopular as = maximumBy (comparing (occurences as)) as

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . (filter (`elem` ltrs)) . (map toLower) . concat
  where
    ltrs = ['a' .. 'z'] ++ ['0' .. '9']

coolestWord :: [String] -> String
coolestWord strs = mostPopular ws
  where
    ws = strs >>= words <$> (map toLower)
