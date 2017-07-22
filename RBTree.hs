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

  insert :: Ord a => a -> RBTree a -> RBTree a
  insert x s = Node B a y b
    where
      ins Leaf = Node R Leaf x Leaf
      ins ss@(Node color sa sy sb)
        | x < y = balance color (ins sa) sy sb
        | x > y = balance color sa sy (ins sb)
        | otherwise = ss
      Node _ a y b = ins s

  balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
  balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
  balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
  balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
  balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
  balance c l x r = Node c l x r


