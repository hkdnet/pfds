import RBTree

main = do
  let nts = [ foldl (flip newInsert) emptyRB [1..x] | x <- [1..100]]
  print nts
  -- let ts = [ foldl (flip insert) emptyRB [1..x] | x <- [1..100]]
  -- print ts
  -- let ots = [ foldl (flip oldInsert) emptyRB [1..x] | x <- [1..100]]
  -- print ots


testRBTree :: IO()
testRBTree
  | member 1 t = print "NG"
  | not $ member 1 t1 = print "NG"
  | otherwise = print "OK"
  where
    t = emptyRB
    t1 = insert 1 emptyRB
    t2 = fromOrdList [1, 2, 3, 4, 5, 6]


