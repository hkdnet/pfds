module SplayHeap where
  data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a) deriving (Eq, Show)

  insert :: Ord a => a -> SplayHeap a -> SplayHeap a
  insert x t = T (smaller x t) x (bigger x t)

  smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
  smaller _ E = E
  smaller pivot (T a x b) =
    if x >= pivot then smaller pivot a
    else T a x (smaller pivot b)

  bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
  bigger _ E = E
  bigger pivot (T a x b) =
    if x <= pivot then bigger pivot b
    else T (bigger pivot a) x b


