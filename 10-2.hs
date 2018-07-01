import BootstrapSeq

main :: IO()
main = do
  -- 11 = 1011(2)
  let tree = One 0 (One (1, 2) (Zero (One (((3, 4), (5, 6)), ((7, 8), (9, 10))) Nil))) :: Seq Int

  let empty = Nil :: Seq Int
  let t1 = cons 1 empty
  let t2 = cons 2 t1
  let t3 = cons 3 t2
  let t4 = cons 4 t3
  print tree
  print t1
  print t2
  print t3
  print t4
  print $ BootstrapSeq.head t3
  print $ BootstrapSeq.tail t3
  print $ BootstrapSeq.tail t4
