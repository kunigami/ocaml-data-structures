open OUnit2;;
open Queue_suspended;;

let testEmptyCheck text_ctx =
  assert_bool "Should return true for empty queue"
  (Queue_suspended.isEmpty Queue_suspended.newEmpty)
;;

let suite =
"suite">:::
 [
  "testEmptyCheck">:: testEmptyCheck;
 ]
;;

let () =
  run_test_tt_main suite
;;
