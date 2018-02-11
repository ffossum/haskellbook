module OnlyNatural where

-- As natural as any
-- competitive bodybuilder
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger n = go 0 n
  where
    go acc Zero = acc
    go acc (Succ a) = go (acc + 1) a

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0 = Nothing
    | otherwise = Just $ go Zero i
    where
      go acc 0 = acc
      go acc x = go (Succ acc) (x - 1)
