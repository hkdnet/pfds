module Queue where
  -- 1, 2, 3, 4, 5,6 -> ([1, 2, 3], [6, 5, 4])
  -- 不変条件 (f, r) においてfが空になるのはrも空であるときのみ
  type Queue a = ([a], [a])

  head :: Queue a -> a
  head (x:_, _) = x
  head ([], _) = error "empty"

  tail :: Queue a -> Queue a
  tail ([_], r) = (reverse r, [])
  tail (_:f, r) = (f, r)
  tail ([], _) = error "empty"

  snoc :: Queue a -> a -> Queue a
  snoc ([], _) x = ([x], []) -- 不変条件よりrも空
  snoc (f, r) x = (f, x:r)
