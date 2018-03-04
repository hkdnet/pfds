import RandomAccessList

drop :: Int -> RList a -> RList a
drop 0 ts = ts
drop n (Zero:ts) = Zero:(Main.drop n ts)
drop n (One t:ts) = drop' n (One t:ts)
  where
    drop' n (One t:ts)
      | n == size t = Zero:ts
      | n > size t = Zero:(Main.drop (n - size t) ts)
      | otherwise  = (One (dropTree n t)):ts

dropTree :: Int -> Tree a -> Tree a
dropTree n t = t
  where
    if n < w `div` 2 then Node w (updateTree i y t1) t2

main =
  putStrLn "a"
