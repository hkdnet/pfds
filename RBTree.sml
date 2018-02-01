use "common.sml";

functor RedBlackSet(Element:Ordered): Set =
struct
  type Elem = Element.T

  datatype Color = R | B
  (* 色、左、根、右、根が削除済みか、自分を含んだ削除されていない要素数、自分を含んだ要素数 *)
  datatype Tree = E | T of Color * Tree * Elem * Tree * bool * int * int
  datatype Digit = One of Elem * Tree | Two of Elem * Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (_, a, y, b, f, _, _)) =
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


  fun toOrdlist s =
    let fun listFromTree' (E, es) = es
          | listFromTree' (T(_, a, _, b, true, _, _), es) = listFromTree' (a, es) @ listFromTree' (b, es)
          | listFromTree' (T(_, a, x, b, _, _, _), es)    = listFromTree' (a, es) @ [x] @ listFromTree' (b, es)
    in listFromTree' (s, []) end

  (* https://github.com/rst76/pfds/blob/master/ch03/ex.3.9.hs *)

  fun liveCountOf E = 0
    | liveCountOf (T(_, _, _, _, _, c, _)) = c

  fun tekitou (t2, a3, t3) =
    let val lc = liveCountOf t2 + liveCountOf t3
    in T(B, t2, a3, t3, false, lc, lc) end

  fun incr (One (a, t), []) = [One(a, t)]
    | incr (One (a1, t1), One(a2, t2)::ps) = Two(a1, t1, a2, t2)::ps
    | incr (One (a1, t1), Two(a2, t2, a3, t3)::ps) = One(a1, t1)::(incr (One (a2, (tekitou (t2, a3, t3))), ps))
    | incr (_, _) = raise Match

  fun add(ps, a) = incr (One(a, E), ps)
    (*
  fun fromOrdList = foldl (link E . (foldl (add, []))
    *)
  fun fromOrdList _ = E
  fun pbalance s = fromOrdList (toOrdlist s)

  fun insert (x, s) =
    let fun ins E = T(R, E, x, E, false, 1, 1)
          | ins (s as T(color, a, y, b, f, ss, t)) =
          if Element.lt(x, y) then T(color, ins a, y, b, f, ss + 1, t + 1)
          else if Element.lt(y, x) then T(color, a, y, ins b, f, ss + 1, t + 1)
          else s
        val T(_, a, y, b, f', ss, t) = ins s
    in T(B, a, y, b, f', ss, t) end

  fun delete (x, s) =
    let fun del E = E
          | del (s as T(color, a, y, b, f, ss, t)) =
          if Element.lt(x, y) then T (color, del a, y, b, f, ss - 1, t)
          else if Element.lt(y, x) then T (color, a, y, del b, f, ss - 1, t)
          else T(color, a, y, b, true, ss, t)
        val T(_, a, y, b, f, ss, t) = del s
    in T(B, a, y, b, f, ss, t) end
end
