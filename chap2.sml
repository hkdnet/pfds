use "common.sml";

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
  val suffixes : 'a Stack -> 'a Stack Stack
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

  fun suffixes([]) = [[]]
    | suffixes(x::xs) = cons (x::xs, suffixes xs)
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
  fun suffixes(Nil) = Nil
    | suffixes(Cons(x, xs)) = Cons(Cons(x, xs), suffixes xs)
end

functor UnbalancedTree (Element : Ordered) =
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  fun member (x, E) = (print "member is called"; false)
    | member (x, s as T(lt, y, rt)) = (
      print "member is called";
      if Element.lt(x, y) then member(x, lt)
      else if Element.lt(y, x) then member(x, rt)
      else true
    )
  fun insert (x, E) = T(E, x, E)
    | insert (x, s as T(a, y, b)) =
        if Element.lt (x, y) then T(insert(x, a), y, b)
        else if Element.lt (y, x) then T(a, y, insert(x, b))
        else s
end

structure MyInt : Ordered =
struct
  type T = int

  fun eq (a, b) = a = b
  fun lt (a, b) = a < b
  fun le (a, b) = eq(a, b) orelse lt (a, b)
end

structure IntUnbalancedTree = UnbalancedTree (MyInt)