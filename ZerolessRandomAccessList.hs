module ZerolessRandomAccessList where
  data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
  data Digit a = One (Tree a) | Two (Tree a) (Tree a) deriving Show

  type RList a = [Digit a]

  empty :: RList a
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
  consTree t ((One t'):ts) = (Two t t'):ts
  consTree t ((Two t1 t2):ts) = (One t):consTree (link t1 t2) ts

  unconsTree :: RList a -> (Tree a, RList a)
  unconsTree [] = error "empty"
  unconsTree [One t] = (t, [])
  unconsTree (Two t1 t2:ts) = (t1, One t2:ts)
  unconsTree (One t:ts) = (t, (Two a b):ts')
    where
      (Node _ a b, ts') = unconsTree ts

  cons :: a -> RList a -> RList a
  cons x ts = consTree (Leaf x) ts

  head :: RList a -> a
  head [] = error "empty"
  head (One (Leaf x):_) = x
  head (Two (Leaf x) _:_) = x
  head (One (Node _ _ _):_) = error "not dense"
  head (Two (Node _ _ _) _:_) = error "not dense"

  tail :: RList a -> RList a
  tail ts = ts'
    where (_, ts') = unconsTree ts

  lookupTree :: Int -> Tree a -> a
  lookupTree 0 (Leaf x) = x
  lookupTree _ (Leaf _) = error "subcrypt"
  lookupTree i (Node w t1 t2) = if i < w `div` 2 then lookupTree i t1 else lookupTree (i - w `div` 2) t2

  updateTree :: Int -> a -> Tree a -> Tree a
  updateTree 0 y (Leaf _) = Leaf y
  updateTree _ _ (Leaf _) = error "subcrypt"
  updateTree i y (Node w t1 t2) =
    if i < w `div` 2 then Node w (updateTree i y t1) t2
    else Node w t1 (updateTree (i - w `div` 2) y t2)

  lookup :: Int -> RList a -> a
  lookup _ [] = error "subcrypt"
  lookup i (One t:ts) =
    if i < size t then lookupTree i t
    else ZerolessRandomAccessList.lookup (i - size t) ts
  lookup i (Two t1 t2:ts) =
    if i < size t1 then lookupTree i t1
    else if i < 2 * (size t1) then lookupTree (i - size t1) t2
      else ZerolessRandomAccessList.lookup (i - (size t1 * 2)) ts

  update :: Int -> a -> RList a -> RList a
  update _ _ [] = error "subcrypt"
  update i y (One t:ts) =
    if i < size t then (One (updateTree i y t)):ts
    else One t:(update (i - size t) y ts)
  update i y (Two t1 t2:ts) =
    if i < size t1 then (Two (updateTree i y t1) t2):ts
    else if i < 2 * (size t1) then (Two t1 (updateTree (i - size t1) y t2)):ts
      else (Two t1 t2):(update (i - (size t1 * 2)) y ts)
