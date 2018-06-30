module BootstrapSeq where
  data Seq a = Nil | Cons a (Seq (a, a))
