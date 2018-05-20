import HMSkewBinaryNumberRandomList

main :: IO()
main = do
  let e0 = empty :: Queue Int
  print e0
  let e1 = snoc e0 1
  let e2 = snoc e1 2
  let e3 = snoc e2 3
  let e4 = snoc e3 4
  print e0
  print e1
  print e2
  print e3
  print $ HMSkewBinaryNumberRandomList.lookup 2 e3
  print e4
  print $ snoc e4 5
