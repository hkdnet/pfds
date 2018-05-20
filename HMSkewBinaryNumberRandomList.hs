module HMSkewBinaryNumberRandomList where
  import SkewBinaryNumber

  data RotationState a = Idle
                       | Reversing Int (RList a) (RList a) (RList a) (RList a)
                       | Appending Int (RList a) (RList a)
                       | Done (RList a) deriving Show
  data Queue a = Q Int (RList a) (RotationState a) Int (RList a) deriving (Show)

  exec :: RotationState a -> RotationState a
  exec (Reversing ok (x:f) f' (y:r) r') = Reversing (ok + 1) f (x:f') r (y:r')
  exec (Reversing ok [] f' [y] r') = Appending ok f' (y:r')
  exec (Appending 0 f' r') = Done r'
  exec (Appending ok (x:f') r') = Appending (ok - 1) f' (x:r')
  exec state = state

  invalidate :: RotationState a -> RotationState a
  invalidate (Reversing ok f f' r r') = Reversing (ok -1) f f' r r'
  invalidate (Appending 0 f' (x:r')) = Done r'
  invalidate (Appending ok f' r') = Appending (ok - 1) f' r'
  invalidate state = state


  exec2 :: Queue a -> Queue a
  exec2 (Q lenf f state lenr r) = exec2' $ exec $ exec state
    where exec2' (Done newf) = Q lenf newf Idle     lenr r
          exec2' newstate    = Q lenf f    newstate lenr r

  check :: Queue a -> Queue a
  check q@(Q lenf f state lenr r) =
    if lenr <= lenf then exec2 q
      else exec2 (Q (lenf + lenr) f (Reversing 0 f [] r []) 0 [])

  empty :: Queue a
  empty = Q 0 [] Idle 0 []

  isEmpty :: Queue a -> Bool
  isEmpty (Q lenf _ _ _ _) = lenf == 0

  snoc :: Queue a -> a -> Queue a
  snoc (Q lenf f state lenr r) x = check (Q lenf f state (lenr+1) (cons x r))

  head :: Queue a -> a
  head (Q _ [] _ _ _) = error "empty"
  head (Q _ fs _ _ _) = SkewBinaryNumber.head fs

  tail :: Queue a -> Queue a
  tail (Q _ [] _ _ _) = error "empty"
  tail (Q lenf fs state lenr r) = check (Q (lenf-1) (SkewBinaryNumber.tail fs) (invalidate state) lenr r)

  lookup :: Int -> Queue a -> a
  lookup i (Q lenf fs state lenr rs) =
    if i < lenf then SkewBinaryNumber.lookup i fs
      else
        if (i - lenf) < lenr then SkewBinaryNumber.lookup (lenr - (i - lenf)) rs -- リバースされてるので lenr からひいてる
          else lookupState (i - lenf - lenr) state

  lookupState :: Int -> RotationState a -> a
  lookupState _ Idle = error "invalid state"
  -- ここを真面目にみればいいんだと思うが
  lookupState i (Reversing _ l1 l2 l3 l4) = SkewBinaryNumber.lookup i l1
  lookupState i (Appending n l1 l2) =
    if i < n then SkewBinaryNumber.lookup i l1 else SkewBinaryNumber.lookup (i-n) l2
  lookupState i (Done l) = SkewBinaryNumber.lookup i l
