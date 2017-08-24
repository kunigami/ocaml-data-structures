open BatInt;;
open SkewBinomialHeap;;
open HeapTestBase;;

module SkewBinomialIntHeap = SkewBinomialHeap(BatInt);;
module SkewBinomialHeapTest = HeapTestBase.MakeTest(SkewBinomialIntHeap);;

let () =
  SkewBinomialHeapTest.run
;;
