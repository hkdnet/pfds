module RandomAccessList where
  data Tree a = Leaf a | Node Int (Tree a) (Tree a)
  data Digit a = Zero | One (Tree a)

  type RList a = [Digit a]

  empty = []

  isEmpty :: RList a -> Bool
  isEmpty [] = True
  isEmpty _ = False

  size :: Tree a -> Int
  size (Leaf _) = 1
  size (Node s _ _) = s

  link :: Tree a -> Tree a -> Tree a
  link a b = Node (size a + size b) a b

  consTree :: Tree a -> RList a -> RList a
  consTree t [] = [One t]
  consTree t (Zero:ts) = (One t):ts
  consTree t ((One t'):ts) = Zero:consTree (link t t') ts
