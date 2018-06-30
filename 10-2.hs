import BootstrapSeq

main :: IO()
main = do
  -- 11 = 1011(2)
  let tree = One 0 (One (1, 2) (Zero (One (((3, 4), (5, 6)), ((7, 8), (9, 10))) Nil)))
  print tree
