import Data.List

snoc :: [a] -> a -> [a]
-- 動きが見たいだけだから雑に
snoc xs y = xs ++ [y]

main = do
  let a = snoc [] 0
  print a
  let b = snoc a 1
  print b
  let c = tail b
  print c
  let d = snoc b 2
  print d
  let e = c ++ d
  print e
  let f = tail c
  print f
  let g =  snoc d 3
  print g
