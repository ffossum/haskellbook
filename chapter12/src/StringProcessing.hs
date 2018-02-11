module StringProcessing where

import Data.Char

-- example GHCi session
-- above the functions

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . (map f) . words
  where
    f = (maybe "a" id) . notThe

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go 0 ws
    where
      ws = words str
      go acc [ ] = acc
      go acc [_] = acc
      go acc (a1:a2:as)
        | a1 == "the" && beginsWithVowel a2 = go (acc + 1) as
        | otherwise                         = go acc (a2:as)

beginsWithVowel :: String -> Bool
beginsWithVowel "" = False
beginsWithVowel (a:_) = isVowel a

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") . toLower

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel