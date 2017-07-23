module RBTree where
  data Color = R | B deriving Show
  data RBTree a = Leaf | Node Color !(RBTree a) a !(RBTree a) deriving Show


  emptyRB :: RBTree a
  emptyRB = Leaf

  member :: Ord a => a -> RBTree a -> Bool
  member _ Leaf = False
  member x (Node _ a y b)
    | x < y = member x a
    | x > y = member x b
    | otherwise = True

  newInsert :: Ord a => a -> RBTree a -> RBTree a
  newInsert x s = Node B a y b
    where
      ins Leaf = Node R Leaf x Leaf
      ins ss@(Node color sa@(Node _ _ saRoot _) sy sb@(Node _ _ sbRoot _))
        | x < sy && x < saRoot = llbalance color (ins sa) sy sb
        | x < sy && x > saRoot = lrbalance color (ins sa) sy sb
        | x < sy = Node color (ins sa) sy sb -- >=にしてllbalanceにまかせても速度はほぼ同じな気がする？
        | x > sy && x < sbRoot = rlbalance color sa sy (ins sb)
        | x > sy && x > sbRoot = rrbalance color sa sy (ins sb)
        | x > sy = Node color sa sy (ins sb)
        | otherwise = ss
      Node _ a y b = ins s

  insert :: Ord a => a -> RBTree a -> RBTree a
  insert x s = Node B a y b
    where
      ins Leaf = Node R Leaf x Leaf
      ins ss@(Node color sa sy sb)
        | x < sy = lbalance color (ins sa) sy sb
        | x > sy = rbalance color sa sy (ins sb)
        | otherwise = ss
      Node _ a y b = ins s

  eq :: Eq a => RBTree a -> RBTree a -> Bool
  eq (Node B xl x xr) (Node B yl y yr) = x == y && eq xl yl && eq xr yr
  eq (Node R xl x xr) (Node R yl y yr) = x == y && eq xl yl && eq xr yr
  eq _ _ = False

  oldInsert :: Ord a => a -> RBTree a -> RBTree a
  oldInsert x s = Node B a y b
    where
      ins Leaf = Node R Leaf x Leaf
      ins ss@(Node color sa sy sb)
        | x < sy = oldBalance color (ins sa) sy sb
        | x > sy = oldBalance color sa sy (ins sb)
        | otherwise = ss
      Node _ a y b = ins s

  fromOrdList :: Ord a => [a] -> RBTree a
  fromOrdList list = fromOrdList' list emptyRB
    where
      fromOrdList' xs t = foldl (flip insert) t xs
  oldFromOrdList :: Ord a => [a] -> RBTree a
  oldFromOrdList list = fromOrdList' list emptyRB
    where
      fromOrdList' xs t = foldl (flip oldInsert) t xs

  oldBalance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
  oldBalance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
  oldBalance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
  oldBalance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
  oldBalance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
  oldBalance c l x r = Node c l x r

  lbalance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
  lbalance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
  lbalance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
  lbalance c l x r = Node c l x r

  llbalance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
  llbalance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
  llbalance c l x r = Node c l x r
  lrbalance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
  lrbalance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
  lrbalance c l x r = Node c l x r

  rbalance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
  rbalance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
  rbalance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
  rbalance c l x r = Node c l x r

  rlbalance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
  rlbalance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
  rlbalance c l x r = Node c l x r
  rrbalance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
  rrbalance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
  rrbalance c l x r = Node c l x r

