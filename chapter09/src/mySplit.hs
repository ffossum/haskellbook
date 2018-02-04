module MySplit where

mySplit :: Char -> String -> [String]
mySplit c s = reverse $ go s []
    where
        go "" res = res
        go s  res = go next (word : res)
            where
                word = takeWhile (/=c) s
                next = dropWhile (==c) . dropWhile (/=c) $ s

myWords :: String -> [String]
myWords = mySplit ' '

myLines :: String -> [String]
myLines = mySplit '\n'
