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

let testRotations queueModule text_ctx =
  let module Queue = (val queueModule: IQueue) in
  let actual = Queue.(
      newEmpty |>
      (* f: [1], r: [], s: [1] *)
      push 1 |>
      (* f: [1], r: [2], s: [] *)
      push 2 |>
      (* Creates lazy stream of [1, rot([], [2], [3])] *)
      (* f: [1, 2, 3], r: [], s: [1, 2, 3] *)
      push 3 |>
      (* Executes rotation -> [1 [2 [3]]] *)
      (* f: [1, 2, 3], r: [4], s: [2, 3] *)
      push 4 |>
      (* f: [1, 2, 3], r: [4, 5], s: [3] *)
      push 5 |>
      (* f: [1, 2, 3], r: [4, 5, 6], s: [] *)
      push 6 |>
      (* Creates lazy stream of [1, rot([2, 3], [4, 5, 6], [7])] *)
      (* f: [1, 2, 3, 4, 5, 6, 7], r: [], s: [1, 2, 3, 4, 5, 6, 7] *)
      push 7 |>
      (* Executes stream -> [1 [2 [rot([3], [4, 5], [6, 7])]]] *)
      (* f: [1, 2, 3, 4, 5, 6, 7], r: [8], s: [2, 3, 4, 5, 6, 7] *)
      push 8 |>
      (* Executes stream -> [1 [2 [3 [rot([], [4], [5, 6, 7])]]]] *)
      (* f: [1, 2, 3, 4, 5, 6, 7], r: [8, 9], s: [3, 4, 5, 6, 7] *)
      push 9 |>
      toList
  ) in
  assert_equal ~msg:"Should return a queue with elements in order"
    actual
    [1; 2; 3; 4; 5; 6; 7; 8; 9]
;;

let getSuite queueModule =
  "suite">:::
   [
    "testSingleInsertion">:: testSingleInsertion queueModule;
    "testMultipleInsertions">:: testMultipleInsertions queueModule;
    "testInsertingAndRemovingMultipleElements">:: testInsertingAndRemovingMultipleElements queueModule;
    "testTest">:: testRotations queueModule;
   ]
;;

let runTests queueModule =
  let suite = getSuite queueModule in
  run_test_tt_main suite
;;
