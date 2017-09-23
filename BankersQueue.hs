module BankersQueue where
  import GHC.List;

  data Queue a = D Int [a] Int [a] deriving (Eq, Show)

  empty :: Queue a
  empty = D 0 [] 0 []

  isEmpty :: Queue a -> Bool
  isEmpty (D 0 _ _ _) = True
  isEmpty (D _ _ _ _) = False

  check :: Queue a -> Queue a
  check q@(D fl f rl r) =
    if rl < fl then q
      else (D (fl + rl) (f ++ reverse r) 0 [])

  cons :: a -> Queue a -> Queue a
  cons x (D 1 f 0 _) = D 1 [x] 1 f -- reverse 不要
  cons x (D fl f rl r) = check $ D (fl + 1) (x:f) rl r

  snoc :: a -> Queue a -> Queue a
  snoc x (D fl f rl r) = check $ D fl f (rl + 1) (x:r)

  head :: Queue a -> a
  head (D _ [] _ _) = error "empty"
  head (D fl (x:fs) rl r) = x

  tail :: Queue a -> Queue a
  tail (D _ [] _ _) = error "empty"
  tail (D fl (x:fs) rl r) = check (D (fl - 1) fs rl r)
