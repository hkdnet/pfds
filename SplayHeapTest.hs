import SplayHeap

main = do
  let heap = E
  let h1 = insert 1 heap

  print h1
  print $ foldl (flip insert) heap [1..10]
  print $ foldl (flip insert) heap $ reverse [1..10]
  print $ foldl (flip insert) heap [1]
  print $ foldl (flip insert) heap [1, 4]
  print $ foldl (flip insert) heap [1, 4, 5]
  print $ foldl (flip insert) heap [1, 4, 5, 3]
  print $ foldl (flip insert) heap [1, 4, 5, 3, 2]


