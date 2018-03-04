import RandomAccessList

-- reverse するが log_n 個なのでセーフ？
create :: Int -> a -> RList a
create n e = reverse $ create' [] 1 n e

create' :: [Digit a] -> Int -> Int -> a -> [Digit a]
create' ls rank n e
  | n == 0 = ls
  | num == 0 = create' (Zero:ls) (rank + 1) n e
  | otherwise = create' ((One (buildTree num e)):ls) (rank + 1) (n - num) e
  where
    base = 2 ^ rank
    num = n `mod` base

buildTree :: Int -> a -> Tree a
buildTree 1 e = Leaf e
buildTree n e = Node n (buildTree (n `div` 2) e) (buildTree (n `div` 2) e)

main :: IO ()
main = do
  print $ create 1 'a'
  -- [One (Leaf 'a')]
  print $ create 2 'a'
  -- [Zero,One (Node 2 (Leaf 'a') (Leaf 'a'))]
  print $ create 3 'a'
  -- [One (Leaf 'a'),One (Node 2 (Leaf 'a') (Leaf 'a'))]
  print $ create 4 'a'
  -- [Zero,Zero,One (Node 4 (Node 2 (Leaf 'a') (Leaf 'a')) (Node 2 (Leaf 'a') (Leaf 'a')))]
  print $ create 5 'a'
  -- [One (Leaf 'a'),Zero,One (Node 4 (Node 2 (Leaf 'a') (Leaf 'a')) (Node 2 (Leaf 'a') (Leaf 'a')))]
