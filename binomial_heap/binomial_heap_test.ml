open BatInt;;
open Binomial_heap;;
open HeapTestBase;;

module IntBinomialHeap = Binomial_heap(BatInt);;
module ScheduledBinomialTest = HeapTestBase.MakeTest(IntBinomialHeap);;

let () =
  ScheduledBinomialTest.run
;;
