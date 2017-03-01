open OUnit2;;
open Queue2;;

let rec listFromQueue (queue: 'a queue): 'a list =
  if Queue2.empty queue then []
  else (Queue2.front queue) :: (listFromQueue (Queue2.pop queue))
;;

let queueFromList (l: 'a list): 'a queue =
  let rec queueFromListRec (l: 'a list): 'a queue = match l with
    | [] -> Queue2.newEmpty
    | (x::xs) -> queueFromListRec(xs) |> Queue2.push x
  in queueFromListRec (List.rev l)
;;

let testEmptyCheck text_ctx =
  assert_bool "Should return true for empty queue"
  (Queue2.empty Queue2.newEmpty)
;;

let testInsertingOnEmptyQueue text_ctx =
  assert_equal ~msg:"Should return a queue with a single element"
    (listFromQueue (Queue2.push 1 Queue2.newEmpty))
    [1]
;;

let testInsertingMultipleElements text_ctx =
  assert_equal ~msg:"Should return a queue with elements in order"
    (listFromQueue (
      Queue2.newEmpty |>
      Queue2.push 1 |>
      Queue2.push 2 |>
      Queue2.push 3
    ))
    [1; 2; 3]
;;

let testACombinationOfOperations text_ctx =
  assert_equal ~msg:"Should return a queue with elements in order"
    (listFromQueue (
      Queue2.newEmpty |>
      Queue2.push 1 |>
      Queue2.push 2 |>
      Queue2.pop |>
      Queue2.push 3
    ))
    [2; 3]
;;

let testConcatenatingTwoQueues text_ctx =
  assert_equal ~msg:"Should concatenate two queues properly"
    (listFromQueue (
      (Queue2.append (queueFromList [1; 2; 3]) (queueFromList [4; 5; 6]))
    ))
    [1; 2; 3; 4; 5; 6]
;;

let suite =
"suite">:::
 [
  "testEmptyCheck">:: testEmptyCheck;
  "testInsertingOnEmptyQueue">:: testInsertingOnEmptyQueue;
  "testInsertingMultipleElements">:: testInsertingMultipleElements;
  "testACombinationOfOperations">:: testACombinationOfOperations;
  "testConcatenatingTwoQueues">:: testConcatenatingTwoQueues;
 ]
;;

let () =
  run_test_tt_main suite
;;
