import RBTree

main :: IO()
main =
  testRBTree

testRBTree :: IO()
testRBTree
  | member 1 t = print "NG"
  | not $ member 1 t1 = print "NG"
  | otherwise = print "OK"
  where
    t = emptyRB
    t1 = insert 1 emptyRB
    t2 = fromOrdList [1, 2, 3, 4, 5, 6]


