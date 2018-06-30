module BootstrapSeq where
  data Seq a = Nil | Zero (Seq(a, a)) | One a (Seq (a, a)) deriving Show
