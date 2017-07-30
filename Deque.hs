
module Deque where
  import GHC.List;
  -- 1, 2, 3, 4, 5,6 -> ([1, 2, 3], [6, 5, 4])
  -- 不変条件 要素が2つ以上あるとき、常にf, rのどちらも空でない
  -- 片方が空になったらもう片方を半分にして reverse する
  -- memo: 半分にするので長さをもっていたほうがよさそう
  type Deque a = (Int, [a], Int, [a])


  head :: Deque a -> a
  head (0, _, 0, _) = error "empty"
  head (0, _, 1, r) = GHC.List.head r
  head (0, _, _, _) = error "制約違反"
  head (_, f, _, _) = GHC.List.head f

  tail :: Deque a -> Deque a
  tail (0, _, 0, _) = error "empty"
  tail (0, _, 1, _) = (0, [], 0, [])
  tail (0, _, _, _) = error "制約違反"
  tail (1, _, rLen, r) = (nfl, nf, nrl, nr)
    where
      nfl = rLen `div` 2
      nrl = rLen - nfl
      nr = take nfl r
      nf = reverse $ drop nfl r
  tail (lLen, _:xs, rLen, r) = (lLen - 1, xs, rLen, r)
  tail (_, [], _, _) = error "制約違反"

  last :: Deque a -> a
  last (0, _, 0, _) = error "empty"
  last (1, r, 0, _) = GHC.List.last r
  last (_, _, 0, _) = error "制約違反"
  last (_, _, _, f) = GHC.List.head f

  init :: Deque a -> Deque a
  init (0, _, 0, _) = error "empty"
  init (1, _, 0, _) = (0, [], 0, [])
  init (_, _, 0, _) = error "制約違反"
  init (fLen, f, 1, _) = (nfl, nf, nrl, nr)
    where
      nfl = fLen `div` 2
      nrl = fLen - nfl
      nf = take nfl f
      nr = reverse $ drop nfl f
  init (fLen, f, rLen, _:xs) = (fLen, f, rLen - 1, xs)
  init (_, _, _, [])  = error "制約違反"
