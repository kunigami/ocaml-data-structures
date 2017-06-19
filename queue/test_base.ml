open IQueue;;
open OUnit2;;

(*
  This file contains tests to the abstract data structure queue (regardless of implementation).
  They should be included in specific implementation test files. They must implement the IQueue
  interface.
*)

let testSingleInsertion queueModule text_ctx =
  let module Queue = (val queueModule: IQueue) in
  let result = Queue.push 10 Queue.newEmpty in
  let resultAsList = Queue.toList result in
  assert_equal
    ~msg:"Should insert one element properly"
    resultAsList
    [10]
;;

let testMultipleInsertions queueModule text_ctx =
  let module Queue = (val queueModule: IQueue) in
  let result = (List.fold_left
    (BatPervasives.flip Queue.push)
    Queue.newEmpty
    [10; 20; 30]
  ) in
  assert_equal
    ~msg:"Should insert many elements properly"
    (Queue.toList result)
    [10; 20; 30]
;;

let testInsertingAndRemovingMultipleElements queueModule text_ctx =
  let module Queue = (val queueModule: IQueue) in
  assert_equal ~msg:"Should return a queue with elements in order"
    Queue.(
      newEmpty |>
      push 1 |>
      push 2 |>
      push 3 |>
      pop |>
      push 4 |>
      toList
    )
    [2; 3; 4]
;;

let getSuite queueModule =
  "suite">:::
   [
    "testSingleInsertion">:: testSingleInsertion queueModule;
    "testMultipleInsertions">:: testMultipleInsertions queueModule;
    "testInsertingAndRemovingMultipleElements">:: testInsertingAndRemovingMultipleElements queueModule;
   ]
;;

let runTests queueModule =
  let suite = getSuite queueModule in
  run_test_tt_main suite
;;
