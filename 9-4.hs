data Digit = One | Two deriving Show
type Nat = [Digit]

inc :: Nat -> Nat
inc [] = [One]
inc (One:ds) = Two:ds
inc (Two:ds) = One:inc ds

dec :: Nat -> Nat
dec x = x

iton :: Int -> Nat
iton 0 = []
iton i = inc $ iton (i - 1)

ntoi :: Nat -> Int
ntoi [] = 0
ntoi n = ntoi' 0 n

ntoi' :: Int -> Nat -> Int
ntoi' _ [] = 0
ntoi' rank (One:ds) = (2 ^ rank) + ntoi' (rank + 1) ds
ntoi' rank (Two:ds) = (2 ^ rank) * 2 + ntoi' (rank + 1 ) ds


testDec :: Int -> Bool
testDec a
  | expected == actual = True
  | otherwise = error "fail"
  where
    expected = a - 1
    actual = ntoi $ dec $ iton a

main :: IO ()
main = do
  print $ iton 1
  print $ iton 2
  print $ iton 3
  print $ iton 4
  print $ ntoi $ iton 1
  print $ ntoi $ iton 2
  print $ ntoi $ iton 3
  print $ ntoi $ iton 4
  print $ testDec 5
