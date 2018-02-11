module EitherLibrary where

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) as = a : as
    f _ as        = as

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right b) bs = b : bs
    f _ bs         = bs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where
    f (Left a) (as, bs)  = (a : as, bs)
    f (Right b) (as, bs) = (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fac _ (Left a)  = fac a
either' _ fbc (Right b) = fbc b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
