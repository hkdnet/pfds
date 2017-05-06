signature Stack =
sig
  type 'a Stack
  val empty : 'a Stack
  val isEmpty : 'a Stack -> bool

  val cons : 'a * 'a Stack -> 'a Stack
  val head : 'a Stack -> 'a
  val tail : 'a Stack -> 'a Stack
  val ++ : 'a Stack * 'a Stack -> 'a Stack
  val update : 'a Stack * int * 'a -> 'a Stack
end

structure MyList: Stack =
struct
  type 'a Stack = 'a list
  val empty = []
  fun isEmpty nil = true
    | isEmpty _ = false
  fun cons (x, xs) = x :: xs
  fun head x = hd x
  fun tail x = tl x
  fun ++ (xs, ys) = if isEmpty xs then ys else cons(head xs, ++ (tail xs, ys))
  fun update([], i, y) = raise Subscript
    | update(x :: xs, 0, y) = y :: xs
    | update(x :: xs, i, y) = update(xs, i - 1, y)
end

structure MyStack: Stack =
struct
  datatype 'a Stack = Nil | Cons of 'a * 'a Stack
  val empty = Nil
  fun isEmpty Nil = true
    | isEmpty _ = false
  fun cons (x, xs) = Cons(x, xs)
  fun head Nil = raise Empty
    | head (Cons(x, xs)) = x
  fun tail Nil = raise Empty
    | tail (Cons(x, xs)) = xs
  fun ++ (xs, ys) = if isEmpty xs then ys else cons(head xs, ++ (tail xs, ys))
  fun update(Nil, i, y) = raise Subscript
    | update(Cons(x, xs), 0, y) = Cons(y, xs)
    | update(Cons(x, xs), i, y) = update(xs, i - 1, y)
end