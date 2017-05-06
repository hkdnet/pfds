signature Stack =
sig
  type 'a Stack
  val empty : 'a Stack
  val isEmpty : 'a Stack -> bool

  val cons : 'a * 'a Stack -> 'a Stack
  val head : 'a Stack -> 'a
  val tail : 'a Stack -> 'a Stack
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
end

structure MyStack: Stack =
struct
  datatype 'a Stack = Nil | CONS of 'a * 'a Stack
  val empty = Nil
  fun isEmpty Nil = true
    | isEmpty _ = false
  fun cons (x, xs) = CONS(x, xs)
  fun head Nil = raise Empty
    | head (CONS(x, xs)) = x
  fun tail Nil = raise Empty
    | tail (CONS(x, xs)) = xs
end