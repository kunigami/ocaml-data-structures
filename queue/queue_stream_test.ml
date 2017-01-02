open OUnit2;;
open Queue_stream;;

(* Convenience method for comparing the result *)
let rec convertQueueToList (queue: 'a queueStream): 'a list =
  if (Queue_stream.isEmpty queue) then []
  else
    let elem = Queue_stream.peek queue in
    elem :: (convertQueueToList (Queue_stream.pop queue))
;;

let testEmptyCheck text_ctx =
  assert_bool "Should return true for empty queue"
  (Queue_stream.isEmpty Queue_stream.newEmpty)
;;

let testConvertingEmptyQueue text_ctx =
  assert_equal ~msg:"Should return an empty list for an empty queue"
  (convertQueueToList Queue_stream.newEmpty)
  []
;;

let testInsertingOnEmptyQueue text_ctx =
  assert_equal ~msg:"Should return a queue with a single element"
  (convertQueueToList (Queue_stream.push 1 Queue_stream.newEmpty))
  [1]
;;

let testInsertingMultipleElements text_ctx =
  assert_equal ~msg:"Should return a queue with elements in order"
    (convertQueueToList (
      Queue_stream.newEmpty |>
      Queue_stream.push 1 |>
      Queue_stream.push 2 |>
      Queue_stream.push 3
    ))
    [1; 2; 3]
;;

let testInsertingAndRemovingMultipleElements text_ctx =
  assert_equal ~msg:"Should return a queue with elements in order"
    (convertQueueToList (
      Queue_stream.newEmpty |>
      Queue_stream.push 1 |>
      Queue_stream.push 2 |>
      Queue_stream.push 3 |>
      Queue_stream.pop |>
      Queue_stream.push 4
    ))
    [2; 3; 4]
;;


let suite =
"suite">:::
 [
  "testEmptyCheck">:: testEmptyCheck;
  "testConvertingEmptyQueue">:: testConvertingEmptyQueue;
  "testInsertingOnEmptyQueue">:: testInsertingOnEmptyQueue;
  "testInsertingMultipleElements">:: testInsertingMultipleElements;
  "testInsertingAndRemovingMultipleElements">:: testInsertingAndRemovingMultipleElements;
 ]
;;

let () =
  run_test_tt_main suite
;;
