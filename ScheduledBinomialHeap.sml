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
  fun insTree(t, $(NIL)) = $(CONS(One t, $(NIL)))
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

  fun mrg (ds1, $(NIL)) = ds1
    | mrg ($(NIL), ds2) = ds2
    | mrg ($(CONS(Zero, ds1)), $(CONS(d, ds2))) = $(CONS(d, mrg(ds1, ds2)))
    | mrg ($(CONS(d, ds1)), $(CONS(Zero, ds2))) = $(CONS(d, mrg(ds1, ds2)))
    | mrg ($(CONS(One t1, ds1)), $(CONS(One t2, ds2))) = $(CONS(Zero, insTree (link(t2, t2),  mrg(ds1, ds2))))

  (* mrg の各ケースについて展開してはやめられればよさそうだが *)
  fun mrgWithList (xs, ds) = mrg (listToStream (map One (xs)), ds)

  fun normalize ($(NIL)) = $(NIL)
    | normalize (ds as $(CONS(_, ds'))) = (normalize ds'; ds)

  fun merge ((ds1, _), (ds2, _)) =
    let val ds = normalize (mrg (ds1, ds2))
    in (ds, []) end

  fun removeMinTree ($(NIL)) = raise Empty
    | removeMinTree ($(CONS(One t, $(NIL)))) = (t, $(NIL))
    | removeMinTree ($(CONS(Zero, ds))) =
      let val (t', ds') = removeMinTree ds in (t', $(CONS(Zero, ds'))) end
    | removeMinTree ($(CONS(One (t as Node (x, _)), ds))) =
      case removeMinTree ds of
        (t' as Node(x', _), ds') =>
          if Elem.leq(x, x') then (t, $(CONS(Zero, ds)))
          else (t', $(CONS(One t, ds')))
  fun findMin (ds, _) =
    let val (Node(x, _), _) = removeMinTree ds in x end
  fun deleteMin(ds, _) =
    let val (Node (x, c), ds') = removeMinTree ds
        val ds'' = mrgWithList(rev c, ds')
    in (normalize ds'', []) end
  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
      let val (t', ts') = removeMinTree ts
      in if Elem.leq(root t, root t') then (t, ts)
         else (t', t::ts')
      end
end

