signature Ordered =
sig
  type T
  val eq : T * T -> bool
  val lt : T * T -> bool
  val le : T * T -> bool
end