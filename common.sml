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

datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
withtype 'a Stream = 'a StreamCell susp

fun listToStream [] = $(NIL)
  | listToStream (x::xs) = $(CONS(x, listToStream xs))

signature Heap =
sig
end

signature Set =
sig
end
