use "common.sml";

functor RedBlackSet(Element:Ordered): Set =
struct
  type Elem = Element.T

  datatype Color = R | B
  datatype Tree = E | T of Color * Tree * Elem * Tree * bool * int
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (_, a, y, b, f, _)) =
    if Element.lt(x, y) then member(x, a)
    else if Element.lt(y, x) then member (x, b)
    else (not f)

  (*
  fun balance (B, T(R, T(R, a, x ,b, f1), y, c, f2), z, d, f3) = T ( R, T(B, a, x, b, f1), y, T(B, c, z, d, f3), f2)
    | balance (B, T(R, a, x, T(R, b, y ,c, f2), f1), z, d, f3) = T ( R, T(B, a, x, b, f1), y, T(B, c, z, d, f3), f2)
    | balance (B, a, x, T(R, T(R, b, y ,c, f2), z, d, f3), f1) = T ( R, T(B, a, x, b, f1), y, T(B, c, z, d, f3), f2)
    | balance (B, a, x, T(R, b, y, T(R, c, z, d, f3), f2), f1) = T ( R, T(B, a, x, b, f1), y, T(B, c, z, d, f3), f2)
    | balance body = T body
  *)

  fun insert (x, s) =
    let fun ins E = T(R, E, x, E, false, 1)
          | ins (s as T(color, a, y, b, f, ss)) =
          if Element.lt(x, y) then T(color, ins a, y, b, f, ss + 1)
          else if Element.lt(y, x) then T(color, a, y, ins b, f, ss + 1)
          else s
        val T(_, a, y, b, f', ss) = ins s
    in T(B, a, y, b, f', ss) end

  fun delete (x, s) =
    let fun del E = E
          | del (s as T(color, a, y, b, f, ss)) =
          if Element.lt(x, y) then T (color, del a, y, b, f, ss - 1)
          else if Element.lt(y, x) then T (color, a, y, del b, f, ss - 1)
          else T(color, a, y, b, true, ss)
        val T(_, a, y, b, f, ss) = del s
    in T(B, a, y, b, f, ss) end
end
