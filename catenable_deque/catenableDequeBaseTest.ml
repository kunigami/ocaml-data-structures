open ICatenableDeque
open OUnit2
open BatList
open DequeTestBase

module MakeTest(CatenableDeque: ICatenableDeque) =
  struct

    module DequeTest = DequeTestBase.MakeTest(CatenableDeque)

    let testConcatenatingTwoDeques text_ctx =
      let deque1 =
        CatenableDeque.(
          newEmpty |>
          pushBack 10 |>
          pushBack 20 |>
          pushBack 30
        ) in
      let deque2 =
        CatenableDeque.(
          newEmpty |>
          pushFront 60 |>
          pushFront 50 |>
          pushFront 40
        ) in
      let result = CatenableDeque.concat deque1 deque2 in
      assert_equal
        ~msg:"Should remove elements properly"
        (DequeTest.toList result)
        [10; 20; 30; 40; 50; 60]

      let testOperationsOnCatenatedDeque text_ctx =
        let deque1 =
          CatenableDeque.(
            newEmpty |>
            pushBack 10 |>
            pushBack 20 |>
            pushBack 30
          ) in
        let deque2 =
          CatenableDeque.(
            newEmpty |>
            pushFront 60 |>
            pushFront 50 |>
            pushFront 40
          ) in
        let catenated = CatenableDeque.concat deque1 deque2 in
        let result =
          catenated |>
          CatenableDeque.pushFront 0 |>
          CatenableDeque.pushBack 70 in
        assert_equal
          ~msg:"Should remove elements properly"
          (DequeTest.toList result)
          [0; 10; 20; 30; 40; 50; 60; 70]

    let getTests =
      let catenableTests = [
        "testConcatenatingTwoDeques">:: testConcatenatingTwoDeques;
        "testOperationsOnCatenatedDeque">:: testOperationsOnCatenatedDeque;
      ] in
      (* All tests to deques are applicable to catenable deques *)
      let dequeTests = DequeTest.getTests
      in List.append catenableTests dequeTests

    let run = run_test_tt_main ("suite">::: getTests)
end;;
