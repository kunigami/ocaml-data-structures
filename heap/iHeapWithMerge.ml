open IHeap;;

module type IHeapWithMerge = sig
  include IHeap
  val merge : heap -> heap -> heap
end
