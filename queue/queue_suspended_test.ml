open OUnit2;;
open Queue_suspended;;

let testEmptyCheck text_ctx =
  assert_bool "Should return true for empty queue"
  (Queue_suspended.isEmpty Queue_suspended.newEmpty)
;;

let assertInvariants (queue: 'a queueSuspended) =
  match queue with (forcedFront, frontSize, lazyFront, rearSize, rear) ->
    let front = Lazy.force lazyFront
    in
      assert_bool
        "Length of front should be reflected in frontSize"
        (
          ((List.length front) == frontSize) &&
          ((List.length rear) == rearSize)
        )        
;;

let queueToList (queue: 'a queueSuspended): 'a list =
  match queue with (forcedFront, frontSize, lazyFront, rearSize, rear) ->
    let front = Lazy.force lazyFront
    in front @ (List.rev rear)

let testSingleInsertion text_ctx =
  let result = (Queue_suspended.push (Queue_suspended.newEmpty) 10)
  in let () = assertInvariants result
  in assert_equal
    ~msg:"Should insert one element properly"
    (queueToList result)
    [10]

let suite =
"suite">:::
 [
  "testEmptyCheck">:: testEmptyCheck;
  "testSingleInsertion">:: testSingleInsertion;
 ]
;;

let () =
  run_test_tt_main suite
;;
