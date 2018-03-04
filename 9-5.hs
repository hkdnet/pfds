import ZerolessRandomAccessList

main :: IO()
main = do
  let list = empty :: RList Int
  print list
  print $ cons 1 list
  print $ cons 1 $ cons 1 list
  print $ cons 1 $ cons 1 $ cons 1 list
  let list6 = cons 1 $ cons 2 $ cons 3 $ cons 4 $ cons 5 $ cons 6 list
  print list6
  print $ ZerolessRandomAccessList.lookup 0 list6
  print $ ZerolessRandomAccessList.lookup 1 list6
  print $ ZerolessRandomAccessList.lookup 2 list6
  print $ ZerolessRandomAccessList.lookup 3 list6
  print $ ZerolessRandomAccessList.lookup 4 list6
  print $ ZerolessRandomAccessList.lookup 5 list6

  let list7 = cons 1 $ cons 2 $ cons 3 $ cons 4 $ cons 5 $ cons 6 $ cons 7 list
  print list7
  print $ ZerolessRandomAccessList.lookup 0 list7
  print $ ZerolessRandomAccessList.lookup 1 list7
  print $ ZerolessRandomAccessList.lookup 2 list7
  print $ ZerolessRandomAccessList.lookup 3 list7
  print $ ZerolessRandomAccessList.lookup 4 list7
  print $ ZerolessRandomAccessList.lookup 5 list7
  print $ ZerolessRandomAccessList.lookup 6 list7
