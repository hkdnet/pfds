use "common.sml";

structure HoodMelvilleQueue: Queue =
struct
  datatype 'a RotationState = Idle
                           | Reversing of int * 'a list * 'a list * 'a list * 'a list
                           | Appending of int * 'a list * 'a list
                           | Done of 'a list
  type 'a Queue = int * 'a list * 'a RotationState * int * 'a list

  fun exec (Reversing (ok, x::f, f', y::r, r')) = Reversing(ok + 1, f, x::f', r, y::r')
    | exec (Reversing(ok, [], f', [y], r')) = Appending (ok, f', y::r')
    | exec (Appending(0, f', r')) = Done r'
    | exec (Appending(ok, x::f', r')) = Appending(ok - 1, f', x::r')
    | exec state = state

  fun invalidate (Reversing(ok, f, f', r, r')) = Reversing(ok -1, f, f', r, r')
    | invalidate(Appending(0, f', x::r')) = Done r'
    | invalidate(Appending(ok, f', r')) = Appending(ok - 1, f', r')
    | invalidate state = state

  fun exec2(lenf, f, state, lenr, r) =
    case exec(exec state) of
         Done newf => (lenf, newf, Idle, lenr, r)
       | newstate => (lenf, f, newstate, lenr, r)

  fun check(q as (lenf, f, state, lenr, r)) =
    if lenr <= lenf then exec2 q
    else let val newstate = Reversing(0, f, [], r, [])
         in exec2(lenf + lenr, f, newstate, 0, []) end

  val empty = (0, [], Idle, 0, [])
  fun isEmpty(lenf, _, _, _, _) = (lenf = 0)

  fun snoc ((lenf, f, state, lenr, r), x) = check(lenf, f, state, lenr+1, x::r)
  fun head (_, [], _, _, _) = raise Empty
    | head (_, x::f, _, _, _) = x
  fun tail (_, [], _, _, _) = raise Empty
    | tail (lenf, x::f, state, lenr, r) = check(lenf -1, f, invalidate state, lenr, r)
end
