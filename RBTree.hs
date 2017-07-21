module RBTree where
  data Color = R | B deriving Show
  data RBTree a = Leaf | Node Color !(RBTree a) a !(RBTree a)

  emptyRB :: RBTree a
  emptyRB = Leaf

  member :: Ord a => a -> RBTree a -> Bool
  member _ Leaf = False
  member x (Node _ a y b)
    | x < y = member x a
    | x > y = member x b
    | otherwise = True
