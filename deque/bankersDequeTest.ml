open BankersDeque;;
open BatPervasives;;
open IDeque;;
open OUnit2;;

module MakeTest(Deque: IDeque) =
  struct

    let rec toList (deque: 'a Deque.t): 'a list =
      if Deque.isEmpty(deque) then []
      else
        let frontElem = Deque.peekFront deque in
        frontElem :: toList(Deque.popFront deque)
    ;;

    let testSingleInsertionBack text_ctx =
      let result = Deque.pushBack 10 Deque.newEmpty in
      assert_equal
        ~msg:"Should insert one element properly"
        (toList result)
        [10]
    ;;

    let testSingleInsertionFront text_ctx =
      let result = Deque.pushFront 10 Deque.newEmpty in
      assert_equal
        ~msg:"Should insert one element properly"
        (toList result)
        [10]
    ;;

    let testMultipleInsertions text_ctx =
      let result =
        Deque.(
          newEmpty |>
          pushBack 10 |>
          pushFront 20 |>
          pushFront 30 |>
          pushBack 0
        ) in
      assert_equal
        ~msg:"Should insert multiple elements properly"
        (toList result)
        [30; 20; 10; 0]
    ;;

    let testRemovalFromFront text_ctx =
      let result =
        Deque.(
          newEmpty |>
          pushBack 10 |>
          pushFront 20 |>
          pushBack 30 |>
          popFront
        ) in
      assert_equal
        ~msg:"Should remove elements properly"
        (toList result)
        [10; 30]
    ;;

    let testRemovalFromEnd text_ctx =
      let result =
        Deque.(
          newEmpty |>
          pushBack 10 |>
          pushFront 20 |>
          pushBack 30 |>
          popBack
        ) in
      assert_equal
        ~msg:"Should remove elements properly"
        (toList result)
        [20; 10]
    ;;

    let testPeekFromFront text_ctx =
      let result =
        Deque.(
          newEmpty |>
          pushBack 10 |>
          pushFront 20 |>
          pushBack 30 |>
          popBack
        ) in
      assert_equal
        ~msg:"Should peek the elements properly"
        (Deque.peekFront result, Deque.peekBack result)
        (20, 10)
    ;;

    let suite =
      "suite">:::
       [
        "testSingleInsertionBack">:: testSingleInsertionBack;
        "testSingleInsertionFront">:: testSingleInsertionFront;
        "testMultipleInsertions">:: testMultipleInsertions;
        "testRemovalFromFront">:: testRemovalFromFront;
        "testRemovalFromEnd">:: testRemovalFromEnd;
        "testPeekFromFront">:: testPeekFromFront;
       ]
    ;;

    let run = run_test_tt_main suite
end;;

module BankerDequeTest = MakeTest(BankersDeque);;

let () =
  BankerDequeTest.run
;;
