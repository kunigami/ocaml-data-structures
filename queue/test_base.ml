open IQueue;;
open OUnit2;;
open MakeQueueUtils;;

(*
  This file contains tests to the abstract data structure queue (regardless of implementation).
  They should be included in specific implementation test files. They must implement the IQueue
  interface.
*)

module MakeTest(Queue: IQueue) =
  struct
    module QueueUtils = MakeQueueUtils(Queue);;

    let testSingleInsertion text_ctx =
      let result = Queue.push 10 Queue.newEmpty in
      let resultAsList = QueueUtils.toList result in
      assert_equal
        ~msg:"Should insert one element properly"
        resultAsList
        [10]
    ;;

    let testMultipleInsertions text_ctx =
      let result = (List.fold_left
        (BatPervasives.flip Queue.push)
        Queue.newEmpty
        [10; 20; 30]
      ) in
      assert_equal
        ~msg:"Should insert many elements properly"
        (QueueUtils.toList result)
        [10; 20; 30]
    ;;

    let testInsertingAndRemovingMultipleElements text_ctx =
      assert_equal ~msg:"Should return a queue with elements in order"
        (Queue.(
          newEmpty |>
          push 1 |>
          push 2 |>
          push 3 |>
          pop |>
          push 4
        ) |>
        QueueUtils.toList
        )
        [2; 3; 4]
    ;;

    let testRotations text_ctx =
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
          push 9
      ) |> QueueUtils.toList in
      assert_equal ~msg:"Should return a queue with elements in order"
        actual
        [1; 2; 3; 4; 5; 6; 7; 8; 9]
    ;;

    let suite =
      "suite">:::
       [
        "testSingleInsertion">:: testSingleInsertion;
        "testMultipleInsertions">:: testMultipleInsertions;
        "testInsertingAndRemovingMultipleElements">:: testInsertingAndRemovingMultipleElements;
        "testRotations">:: testRotations;
       ]
    ;;

    let run = run_test_tt_main suite
end;;
