module ReadingComprehension where

import           Reader

-- [1]
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- [2]
asks :: (r -> a) -> Reader r a
asks f = Reader f
