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

  unconsTree :: RList a -> (Tree a, RList a)
  unconsTree [] = error "empty"
  unconsTree [One t] = (t, [])
  unconsTree (One t:ts) = (t, Zero:ts)
  unconsTree (Zero:ts) = (t1, One t2:ts')
    where
      (Node _ t1 t2, ts') = unconsTree ts

  cons :: a -> RList a -> RList a
  cons x ts = consTree (Leaf x) ts

  head :: RList a -> a
  head ts = x
    where (Leaf x, _) = unconsTree ts

  tail :: RList a -> RList a
  tail ts = ts'
    where (_, ts') = unconsTree ts

  lookupTree :: Int -> Tree a -> a
  lookupTree 0 (Leaf x) = x
  lookupTree i (Leaf x) = error "subcrypt"
  lookupTree i (Node w t1 t2) = if i < w `div` 2 then lookupTree i t1 else lookupTree (i - w `div` 2) t2

  updateTree :: Int -> a -> Tree a -> Tree a
  updateTree 0 y (Leaf x) = Leaf y
  updateTree i y (Leaf x) = error "subcrypt"
  updateTree i y (Node w t1 t2) =
    if i < w `div` 2 then Node w (updateTree i y t1) t2
    else Node w t1 (updateTree (i - w `div` 2) y t2)

  lookup :: Int -> RList a -> a
  lookup i [] = error "subcrypt"
  lookup i (Zero:ts) = RandomAccessList.lookup i ts
  lookup i (One t:ts) =
    if i < size t then lookupTree i t
    else RandomAccessList.lookup (i - size t) ts

  update :: Int -> a -> RList a -> RList a
  update i y [] = error "subcrypt"
  update i y (Zero:ts) = RandomAccessList.update i y ts
  update i y (One t:ts) =
    if i < size t then (One (updateTree i y t)):ts
    else One t:(update (i - size t) y ts)
