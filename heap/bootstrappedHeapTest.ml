open BatInt;;
open BootstrappedHeap;;
open HeapTestBase;;
open SkewBinomialHeap;;

module BootstrappedIntHeap = BootstrappedHeap (BatInt) (SkewBinomialHeap);;
module BootstrappedHeapTest = HeapTestBase.MakeTest(BootstrappedIntHeap);;

let () =
  BootstrappedHeapTest.run
;;
