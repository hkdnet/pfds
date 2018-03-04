import RandomAccessList

drop :: Int -> RList a -> RList a
drop 0 ts = ts
drop n (Zero:ts) = Zero:(Main.drop n ts)
drop n (One t:ts) = drop' n (One t:ts)
  where
    drop' n (One t:ts)
      | n > size t = Zero:(Main.drop (n - size t) ts)
      | otherwise  = (One (dropTree n t)):ts
    dropTree n t = t

main =
  putStrLn "a"
