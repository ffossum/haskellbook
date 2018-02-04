module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
    \ symmetry?"

sentences = firstSen ++ secondSen
    ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
myLines :: String -> [String]
myLines s = reverse $ go s []
    where
        go "" res = res
        go s  res = go next (word : res)
            where
                word = takeWhile (/='\n') s
                next = dropWhile (=='\n') . dropWhile (/='\n') $ s

-- What we want 'myLines sentences'
-- to equal
shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

main :: IO ()
main =
    print $
    "Are they equal? "
    ++ show (myLines sentences
    == shouldEqual)