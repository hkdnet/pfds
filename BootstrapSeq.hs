module BootstrapSeq where
  data Seq a = Nil | Zero (Seq(a, a)) | One a (Seq (a, a)) deriving Show

  cons :: a -> Seq a -> Seq a
  cons a Nil = One a Nil
  cons a (Zero s) = One a s
  cons a (One e s) = Zero (cons (a, e) s)

  head :: Seq a -> a
  head s = let (e, _) = uncons s in e
  tail :: Seq a -> Seq a
  tail s = let (_, s') = uncons s in s'


  uncons :: Seq a -> (a, Seq a)
  uncons Nil = error "empty"
  uncons (One a Nil) = (a, Nil)
  uncons (One a s) = (a, (Zero s))
  uncons (Zero s) = let ((x, y), s') = uncons s in (x, (One y s'))
