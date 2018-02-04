module ThyFearfulSymmetry where

myWords :: String -> [String]
myWords s = reverse $ go s []
    where
        go "" res = res
        go s  res = go next (word : res)
            where
                word = takeWhile (/=' ') s
                next = dropWhile (==' ') . dropWhile (/=' ') $ s