open BatInt;;
open LeftistHeap;;
open HeapTestBase;;

module IntLeftistHeap = LeftistHeap(BatInt);;
module LeftistHeapTest = HeapTestBase.MakeTest(IntLeftistHeap);;

let () =
  LeftistHeapTest.run
;;
