use "common.sml";

signature Heap =
sig
  structure Elem: Ordered

  type Heap

  val empty : Heap
  val isEmpety : Heap -> bool

  val insert : Elem.T * Heap -> Heap
  val merge : Heap * Heap -> Heap
  val findMin : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end