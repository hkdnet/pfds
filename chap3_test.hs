import RBTree

main :: IO()
main =
  testRBTree

testRBTree :: IO()
testRBTree =
  if member 1 t then
    print "NG"
  else
    print "OK"
  where
    t = emptyRB

