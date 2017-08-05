import Deque

main = do
  let emptyDeque = D 0 [] 0 []
  let r1 = D 0 [] 1 [1]
  if Deque.tail r1 == emptyDeque
    then print "OK"
    else print "NG"
  let l1r3 = D 1 [1] 3 [4, 3, 2]
  if Deque.tail l1r3 == D 1 [2] 2 [4, 3]
    then print "OK"
    else print "NG"
  let l2r3 = D 2 [1, 2] 3 [5, 4, 3]
  if Deque.tail l2r3 == D 1 [2] 3 [5, 4, 3]
    then print "OK"
    else print "NG"
  let l3r1 = D 3 [1, 2, 3] 1 [4]
  if Deque.tail l3r1 == D 2 [2, 3] 1 [4]
    then print "OK"
    else print "NG"
  if Deque.init r1 == emptyDeque
    then print "OK"
    else print "NG"
  let l1r3 = D 1 [1] 3 [4, 3, 2]
  if Deque.init l1r3 == D 1 [1] 2 [3, 2]
    then print "OK"
    else print "NG"
  let l2r3 = D 2 [1, 2] 3 [5, 4, 3]
  if Deque.init l2r3 == D 2 [1, 2] 2 [4, 3]
    then print "OK"
    else print "NG"
  let l3r1 = D 3 [1, 2, 3] 1 [4]
  if Deque.init l3r1 == D 1 [1] 2 [3, 2]
    then print "OK"
    else print "NG"

  let unbalanced = D 16 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16] 1 [17]
  if Deque.check unbalanced == D 8 [1, 2, 3, 4, 5, 6, 7, 8] 9 [17, 16, 15, 14, 13, 12, 11, 10, 9]
    then print "OK"
    else print "NG"

