open OUnit2;;
open Queue_suspended;;

(* -----------------------------------------------------------------------------
 * Helper functions
 * -----------------------------------------------------------------------------
 *)

let assertInvariants (queue: 'a queueSuspended) =
  match queue with (forcedFront, frontSize, lazyFront, rearSize, rear) ->
    let front = Lazy.force lazyFront
    in let () = assert_bool
      "Length of front should be reflected in frontSize"
      ((List.length front) == frontSize)
    in let () = assert_bool
      "Length of rear should be reflected in rearSize"
      ((List.length rear) == rearSize)
    in let () = assert_bool
      "frontSize has to be greater or equal than rearSize"
      (frontSize >= rearSize)
    in ()
;;

let queueToList (queue: 'a queueSuspended): 'a list =
  match queue with (forcedFront, frontSize, lazyFront, rearSize, rear) ->
    let front = Lazy.force lazyFront
    in front @ (List.rev rear)

let listToQueue (xs: 'a list): 'a queueSuspended =
  (* forcedFront doesn't have to be equal to lazyFront, but if lazyFront is not
  empty, so is forcedFront, so we need at least one element *)
  ([List.hd xs], List.length xs, lazy xs, 0, [])

(* -----------------------------------------------------------------------------
 * Tests
 * -----------------------------------------------------------------------------
 *)

let testEmptyCheck text_ctx =
  assert_bool "Should return true for empty queue"
  (Queue_suspended.isEmpty Queue_suspended.newEmpty)
;;

let testSingleInsertion text_ctx =
  let result = Queue_suspended.push Queue_suspended.newEmpty 10
  in let () = assertInvariants result
  in assert_equal
    ~msg:"Should insert one element properly"
    (queueToList result)
    [10]
;;

let testMultipleInsertions text_ctx =
  let result = (List.fold_left
    Queue_suspended.push
    Queue_suspended.newEmpty
    [10; 20; 30]
  )
  in let () = assertInvariants result
  in assert_equal
    ~msg:"Should insert many element properly"
    (queueToList result)
    [10; 20; 30]
;;

let testSingleRemoval text_ctx =
  let initialQueue = listToQueue [10; 11; 12]
  in let result = Queue_suspended.pop initialQueue
  in let () = assertInvariants result
  in assert_equal
    ~msg:"Should remove an element properly"
    (queueToList result)
    [11; 12]
;;

let testCombinationOfOperations text_ctx =
  let q0 = Queue_suspended.newEmpty
  in let q1 = Queue_suspended.push q0 10
  in let q2 = Queue_suspended.push q1 11
  in let q3 = Queue_suspended.pop q2
  in let q4 = Queue_suspended.push q3 12
  in let q5 = Queue_suspended.pop q4
  in let q6 = Queue_suspended.push q5 13
  in let () = assertInvariants q6
  in assert_equal
    ~msg:"Should perform insertions/removals properly"
    (queueToList q6)
    [12; 13]
;;

let testPeek text_ctx =
  let q0 = listToQueue [10; 11; 12]
  in let e = Queue_suspended.peek q0
  in let () = assert_equal
    ~msg:"Should return the first element of the queue"
    e
    10
  in assert_equal
    ~msg:"Peek should not mutate the queue"
    (queueToList q0)
    [10; 11; 12]
;;

let suite =
"suite">:::
 [
  "testEmptyCheck">:: testEmptyCheck;
  "testSingleInsertion">:: testSingleInsertion;
  "testMultipleInsertions">:: testMultipleInsertions;
  "testSingleRemoval">:: testSingleRemoval;
  "testCombinationOfOperations">:: testCombinationOfOperations;
  "testPeek">:: testPeek;
 ]
;;

let () =
  run_test_tt_main suite
;;
