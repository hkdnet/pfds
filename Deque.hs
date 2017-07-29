module Deque where
  -- 1, 2, 3, 4, 5,6 -> ([1, 2, 3], [6, 5, 4])
  -- 不変条件 要素が2つ以上あるとき、常にf, rのどちらも空でない
  -- 片方が空になったらもう片方を半分にして reverse する
  type Deque a = ([a], [a])

  head :: Deque a -> a
  head (x:_, _) = x
  head ([], [x]) = x
  head ([], []) = error "empty"
  head ([], _) = error "制約違反"

