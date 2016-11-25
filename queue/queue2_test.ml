open OUnit2;;
open Queue2;;

let rec convertQueueToList (queue: 'a queue): 'a list =
  if Queue2.empty queue then []
  else (Queue2.front queue) :: (convertQueueToList (Queue2.pop queue))
;;

let testEmptyCheck text_ctx =
  assert_bool "Should return true for empty queue"
  (Queue2.empty Queue2.newEmpty)
;;

let testInsertingOnEmptyQueue text_ctx =
  assert_equal ~msg:"Should return a queue with a single element"
    (convertQueueToList (Queue2.push 1 Queue2.newEmpty))
    [1]
;;

let testInsertingMultipleElements text_ctx =
  assert_equal ~msg:"Should return a queue with elements in order"
    (convertQueueToList (
      Queue2.newEmpty |>
      Queue2.push 1 |>
      Queue2.push 2 |>
      Queue2.push 3
    ))
    [1; 2; 3]
;;

let testACombinationOfOperations text_ctx =
  assert_equal ~msg:"Should return a queue with elements in order"
    (convertQueueToList (
      Queue2.newEmpty |>
      Queue2.push 1 |>
      Queue2.push 2 |>
      Queue2.pop |>
      Queue2.push 3
    ))
    [2; 3]
;;

let suite =
"suite">:::
 [
  "testEmptyCheck">:: testEmptyCheck;
  "testInsertingOnEmptyQueue">:: testInsertingOnEmptyQueue;
  "testInsertingMultipleElements">:: testInsertingMultipleElements;
  "testACombinationOfOperations">:: testACombinationOfOperations;
 ]
;;

let () =
  run_test_tt_main suite
;;
