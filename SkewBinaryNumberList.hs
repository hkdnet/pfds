module SkewBinaryNumberList where
  -- List of weight ( sparse )
  type Nat = [Int]

  inc :: Nat -> Nat
  inc ws@(w1:w2:rest) = if w1 == w2 then (1+w1+w2):rest else 1:ws
  inc ws = 1:ws

  dec :: Nat -> Nat
  dec (1:ws) = ws
  dec (w:ws) = (w `div` 2):(w `div` 2):ws -- w が 2 の場合に対応。
  -- w が 0 の場合は存在しない


