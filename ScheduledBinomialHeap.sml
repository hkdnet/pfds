use "common.sml";

functor ScheduledBinomialHeap(Element : Ordered): Heap =
struct
  structure Elem = Element
  datatype Tree = Node of Elem.T * Tree list
  datatype Digit = Zero | One of Tree
  type Schedule = Digit Stream list
  type Heap = Digit Stream * Schedule

  fun root (Node (x, c)) = x
  fun link(t1 as Node(x1, c1), t2 as Node(x2, c2)) =
    if Elem.leq (x1, x2) then Node (x1, t2::c1)
    else Node (x2, t1::c2)
  fun lazy insTree(t, $(NIL)) = $(CONS(One t, $(NIL)))
         | insTree(t, $(CONS(Zero, ds))) = $(CONS(One t, ds))
         | insTree(t, $(CONS(One t', ds))) = $(CONS(Zero, insTree(link(t, t'), ds)))
  (* exec -> Schedule -> Schedule *)
  fun exec [] = []
    | exec ($(CONS(One t, _)) :: sched) = sched
    | exec ($(CONS(Zero, job)) :: sched) = job::sched
    | exec (_ :: sched) = raise Match
  (* non-exhaustive って言われるけど...
   * exec が Schedule を受け取るなら exhaustiveなはず？
   *)

  fun insert (x, (ds, sched)) =
    let val ds' = insTree (Node (x, []), ds)
    in (ds', exec (exec (ds' :: sched))) end (* 償却コストが2なので2回 exec する *)

  (*
   * とりあえずコメントアウト
  fun mrg (ts1, []) = ts1
    | mrg ([], ts2) = ts2
    | mrg (ts1 as t1::ts1', ts2 as t2::ts2') =
      if      rank t1 < rank t2 then t1 :: mrg(ts1', ts2)
      else if rank t2 < rank t1 then t2 :: mrg(ts1, ts2')
      else    insTree(link(t1, t2), mrg(ts1', ts2'))

  fun lazy merge ($ts1, $ts2) = $(mrg (ts1, ts2))

  fun lazy deleteMin($ts) =
    let val(Node (_, x, ts1), ts2) = removeMinTree ts
    in $(mrg (rev ts1, ts2)) end
  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
      let val (t', ts') = removeMinTree ts
      in if Elem.leq(root t, root t') then (t, ts)
         else (t', t::ts')
      end

  fun findMin ($ts) =
    let val (t, _) = removeMinTree ts in root t end
  *)
end

