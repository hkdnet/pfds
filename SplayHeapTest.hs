import SplayHeap

main = do
  let heap = E
  let h1 = insert 1 heap

  print h1
  print $ foldl (flip insert ) heap [1..10]


