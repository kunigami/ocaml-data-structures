module type IHeap = sig
  type tv;;
  type heap;;

  val empty : heap
  val isEmpty : heap -> bool
  (*
    Inserts an element inside a heap.
  *)
  val insert : tv -> heap -> heap
  val findMin : heap -> tv
  val deleteMin : heap -> heap
end
