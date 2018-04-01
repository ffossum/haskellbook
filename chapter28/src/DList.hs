{-# LANGUAGE FlexibleContexts #-}

module DList where

import           Criterion.Main

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

instance Show a => Show (DList a) where
  show dl = "DL " ++ show (toList dl)

{-# INLINE empty #-}
empty :: DList a
empty = DL $ id

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton a = DL $ (a :)

{-# INLINE toList #-}
toList :: DList a -> [a]
toList (DL l) = l []

infixr `cons`

{-# INLINE cons #-}
cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)

infixl `snoc`

{-# INLINE snoc #-}
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))

{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append as bs = DL (unDL as . unDL bs)

as = cons 1 (cons 2 empty)

bs = cons 3 (cons 4 empty)

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $ whnf schlemiel 123456
    , bench "concat dlist" $ whnf constructDlist 123456
    ]
