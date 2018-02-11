module BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z =
  case f z of
    Nothing        -> Leaf
    Just (l, b, r) -> Node (unfold f l) b (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f a
      | a < n = Just (a + 1, a, a + 1)
      | otherwise = Nothing
