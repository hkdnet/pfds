import ZerolessRandomAccessList

main :: IO()
main = do
  let list = empty :: RList Int
  print list
  print $ cons 1 list
  print $ cons 1 $ cons 1 list
  print $ cons 1 $ cons 1 $ cons 1 list
