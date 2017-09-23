import BankersQueue
import Control.Monad

main = do
  let limit = 100000
  let xs = foldr BankersQueue.snoc BankersQueue.empty [1..limit]
  let ys = foldr (\a -> \bs -> BankersQueue.tail bs) xs [1..limit]
  print ys
