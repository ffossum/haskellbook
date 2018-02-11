module Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z =
  case f z of
    Nothing     -> []
    Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr g
    where g x = Just (x, f x)