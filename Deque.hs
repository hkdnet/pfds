module Deque where
  import GHC.List;
  -- 1, 2, 3, 4, 5,6 -> ([1, 2, 3], [6, 5, 4])
  -- 不変条件 要素が2つ以上あるとき、常にf, rのどちらも空でない
  -- 片方が空になったらもう片方を半分にして reverse する
  -- memo: 半分にするので長さをもっていたほうがよさそう
  data Deque a = D Int [a] Int [a] deriving (Eq, Show)


  head :: Deque a -> a
  head (D 0 _ 0 _) = error "empty"
  head (D 0 _ 1 r) = GHC.List.head r
  head (D 0 _ _ _) = error "制約違反"
  head (D _ f _ _) = GHC.List.head f

  tail :: Deque a -> Deque a
  tail (D 0 _ 0 _) = error "empty"
  tail (D 0 _ 1 _) = D 0 [] 0 []
  tail (D 0 _ _ _) = error "制約違反"
  tail (D 1 _ rLen r) = D nfl nf nrl nr
    where
      nfl = rLen `div` 2
      nrl = rLen - nfl
      nr = take nfl r
      nf = reverse $ drop nfl r
  tail (D lLen (_:xs) rLen r) = D (lLen - 1) xs rLen r
  tail (D _ [] _ _) = error "制約違反"

  last :: Deque a -> a
  last (D 0 _ 0 _) = error "empty"
  last (D 1 r 0 _) = GHC.List.last r
  last (D _ _ 0 _) = error "制約違反"
  last (D _ _ _ f) = GHC.List.head f

  init :: Deque a -> Deque a
  init (D 0 _ 0 _) = error "empty"
  init (D 1 _ 0 _) = D 0 [] 0 []
  init (D _ _ 0 _) = error "制約違反"
  init (D fLen f 1 _) = D nfl nf nrl nr
    where
      nfl = fLen `div` 2
      nrl = fLen - nfl
      nf = take nfl f
      nr = reverse $ drop nfl f
  init (D fLen f rLen (_:xs)) = D fLen f (rLen - 1) xs
  init (D _ _ _ [])  = error "制約違反"
