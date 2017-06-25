open BatInt;;
open Scheduled_binomial_heap;;
open HeapTestBase;;

module IntBinomialHeap = Scheduled_binomial_heap(BatInt);;
module ScheduledBinomialTest = HeapTestBase.MakeTest(IntBinomialHeap);;

let () =
  ScheduledBinomialTest.run
;;
