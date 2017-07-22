import RBTree

main :: IO()
main =
  testRBTree

testRBTree :: IO()
testRBTree
  | member 1 t = print "NG"
  | not $ member 1 tt = print "NG"
  | otherwise = print "OK"
  where
    t = emptyRB
    tt = insert 1 emptyRB
