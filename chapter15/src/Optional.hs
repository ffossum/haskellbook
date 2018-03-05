module Optional where

import           Data.Monoid

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a1) (Only a2) = Only $ a1 `mappend` a2
  mappend Nada a              = a
  mappend a Nada              = a

run :: IO ()
run = do
  putStrLn $ show $ Only (Sum 1) `mappend` Only (Sum 1)
  putStrLn $ show $ Only (Product 4) `mappend` Only (Product 2)
