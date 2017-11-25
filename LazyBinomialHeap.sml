(* http://d.hatena.ne.jp/eldesh/20110704/1309759499 *)
(* https://www.cs.cmu.edu/~rwh/introsml/core/lazydata.htm *)

Control.lazysml := true;
open Lazy

fun force ($ x) = x

signature Ordered =
sig
  type T
  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature Heap =
sig
end

functor LazyBinomialHeap(Element : Ordered): Heap =
struct
  structure Elem = Element
  datatype Tree = Node of int * Elem.T * Tree list
  type Heap = Tree list susp

  val empty = $ []
  fun isEmpeyt($ ts) = null ts
  fun rank (Node (r, x, c)) = r
  fun root (Node (r, x, c)) = x
  fun link(t1 as Node(r, x1, c1), t2 as Node(_, x2, c2)) =
    if Elem.leq (x1, x2) then Node (r + 1, x1, t2::c1)
    else Node (r + 1, x2, t1::c2)
  fun insTree(t, []) = [t]
    | insTree(t, ts as t'::ts') =
      if rank t < rank t' then t::ts
      else insTree(link (t, t'), ts')
  fun mrg (ts1, []) = ts1
    | mrg ([], ts2) = ts2
    | mrg (ts1 as t1::ts1', ts2 as t2::ts2') =
      if      rank t1 < rank t2 then t1 :: mrg(ts1', ts2)
      else if rank t2 < rank t1 then t2 :: mrg(ts1, ts2')
      else    insTree(link(t1, t2), mrg(ts1', ts2'))

  fun lazy insert (x, $ts) = $(insTree (Node (0, x, []), ts))
  fun lazy merge ($ts1, $ts2) = $(mrg (ts1, ts2))

  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
      let val (t', ts') = removeMinTree ts
      in if Elem.leq(root t, root t') then (t, ts)
         else (t', t::ts')
      end

  fun findMin ($ts) =
    let val (t, _) = removeMinTree ts in root t end

  fun lazy deleteMin($ts) =
  let val(Node (_, x, ts1), ts2) = removeMinTree ts
  in $(mrg (rev ts1, ts2)) end
end

