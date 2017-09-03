module SplayHeap where
  -- 左は小さいか等しい
  -- 右は要素より大きい
  data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a) deriving (Eq, Show)

  insert :: Ord a => a -> SplayHeap a -> SplayHeap a
  insert x t = T (smaller x t) x (bigger x t)

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

  smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
  smaller _ E = E
  smaller pivot t@(T a x _) =
    if x > pivot then smaller pivot a
    else smaller' pivot t
  smaller' :: Ord a => a -> SplayHeap a -> SplayHeap a
  smaller' _ E = error "制約違反"
  smaller' _ (T a x E) = T a x E
  smaller' pivot (T a x (T b1 y b2)) =
    if y > pivot then T a x (smaller pivot b1)
    else T (T a x b1) y (smaller pivot b2)

  findMin (T E x _) = x
  findMin (T a x b) = findMin a

  deleteMin (T E x b) = b
  deleteMin (T (T E _ b) y c) = T b y c
  deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)
