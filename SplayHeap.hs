module SplayHeap where
  data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a) deriving (Eq, Show)

  insert :: Ord a => a -> SplayHeap a -> SplayHeap a
  insert x t = T (smaller x t) x (bigger x t)

  smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
  smaller _ E = E
  smaller pivot t@(T a x _) =
    if x >= pivot then smaller pivot a
    else smaller' pivot t
  smaller' :: Ord a => a -> SplayHeap a -> SplayHeap a
  smaller' _ E = error "制約違反"
  smaller' _ (T a x E) = T a x E
  smaller' pivot (T a x (T b1 y b2)) =
    if y >= pivot then T a x (smaller pivot b1)
    else T (T a x b1) y (smaller pivot b2)

  bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
  bigger _ E = E
  bigger pivot t@(T _ x b) =
    if x <= pivot then bigger pivot b
    else bigger' pivot t
  bigger' :: Ord a => a -> SplayHeap a -> SplayHeap a
  bigger' _ E = error "制約違反"
  bigger' _ (T E x b) = T E x b
  bigger' pivot (T (T a1 y a2) x b) =
    if y <= pivot then T (bigger pivot a2) x b
    else T (bigger pivot a1) y (T a2 x b)
