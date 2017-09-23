import BankersQueue
import Control.Monad

main = do
  let xs = foldr BankersQueue.snoc BankersQueue.empty [1..100]
  let ys = foldr (\a -> \bs -> BankersQueue.tail bs) xs [1..100]
  print ys
