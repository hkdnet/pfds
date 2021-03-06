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

  print "start test"

  let expectedBigger  = T (T (T (T E 1 E) 2 (T E 3 E)) 4 (T E 5 E)) 6 (T E 7 E)
  let expectedSmaller = T (T E 1 E) 2 (T (T E 3 E) 4 (T (T E 5 E) 6 (T E 7 E)))
  if bigger 0 (T (T (T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E) 6 E) 7 E) == expectedBigger
    then print "ok"
    else print $ bigger 0 (T (T (T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E) 6 E) 7 E)

  if smaller 8 (T E 1 (T E 2 (T E 3 (T E 4 (T E 5 (T E 6 (T E 7 E))))))) == expectedSmaller
    then print "ok"
    else print $ smaller 8 (T E 1 (T E 2 (T E 3 (T E 4 (T E 5 (T E 6 (T E 7 E))))))) 

  if smaller 4 (T E 1 (T E 2 (T E 3 (T E 4 (T E 5 (T E 6 (T E 7 E))))))) == T (T E 1 E) 2 (T (T E 3 E) 4 E)
    then print "ok"
    else print $ smaller 4 (T E 1 (T E 2 (T E 3 (T E 4 (T E 5 (T E 6 (T E 7 E))))))) 
