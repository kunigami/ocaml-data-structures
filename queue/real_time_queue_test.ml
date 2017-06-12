open OUnit2;;
open Real_time_queue;;
open BatPervasives;;

let testSingleInsertion text_ctx =
  let result = Real_time_queue.push 10 Real_time_queue.newEmpty in
  let resultAsList = Real_time_queue.toList result in
  assert_equal
    ~msg:"Should insert one element properly"
    resultAsList
    [10]
;;

let testMultipleInsertions text_ctx =
  let result = (List.fold_left
    (BatPervasives.flip Real_time_queue.push)
    Real_time_queue.newEmpty
    [10; 20; 30]
  ) in
  let () = List.iter (Printf.printf "%d;") (Real_time_queue.toList result) in
  assert_equal
    ~msg:"Should insert many elements properly"
    (Real_time_queue.toList result)
    [10; 20; 30]
;;

let testInsertingAndRemovingMultipleElements text_ctx =
  assert_equal ~msg:"Should return a queue with elements in order"
    Real_time_queue.(
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


let suite =
"suite">:::
 [
  "testSingleInsertion">:: testSingleInsertion;
  "testMultipleInsertions">:: testMultipleInsertions;
  "testInsertingAndRemovingMultipleElements">:: testInsertingAndRemovingMultipleElements;
 ]
;;

let () =
  run_test_tt_main suite
;;
