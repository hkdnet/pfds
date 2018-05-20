module SkewBinaryNumber where
  import Prelude hiding (lookup)
  -- List of weight ( sparse )
  type Nat = [Int]

  inc :: Nat -> Nat
  inc ws@(w1:w2:rest) = if w1 == w2 then (1+w1+w2):rest else 1:ws
  inc ws = 1:ws

  dec :: Nat -> Nat
  dec (1:ws) = ws
  dec (w:ws) = (w `div` 2):(w `div` 2):ws -- w が 2 の場合に対応。
  -- w が 0 の場合は存在しない

  type RList a = [(Int, Tree a)]
  data Tree a = Leaf a | Node a (Tree a) (Tree a)

  cons :: a -> RList a -> RList a
  cons x ts@((w1,t1):(w2,t2):rest) = if w1 == w2 then (1+w1+w2, Node x t1 t2):rest else (1, Leaf x):ts
  cons x ts = (1, Leaf x):ts

  head :: RList a -> a
  head ((1, Leaf x):_) = x
  head ((w, Node x _ _):_) = x

  tail :: RList a -> RList a
  tail ((1, Leaf _):ts) = ts
  tail ((w, Node _ t1 t2):ts) = (w `div` 2, t1):(w `div` 2, t2):ts

  lookup :: Int -> RList a -> a
  lookup i ((w,t):ts) = if i < w then lookupTree w i t else lookup (i-w) ts

  lookupTree :: Int -> Int -> Tree a -> a
  lookupTree 1 0 (Leaf x) = x
  lookupTree w 0 (Node x _ _) = x
  lookupTree w i (Node _ t1 t2) =
    if i < w `div` 2 then lookupTree (w `div` 2) (i - 1) t1 -- 真ん中を飛ばしたので -1
      else lookupTree (w `div` 2) (i - 1 - w `div` 2) t2 -- 真ん中と左treeを飛ばしたので -1 - w/2
