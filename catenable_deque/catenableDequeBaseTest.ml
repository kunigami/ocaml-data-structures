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
          ~msg:"Should concatenate deques properly"
          (DequeTest.toList result)
          [0; 10; 20; 30; 40; 50; 60; 70]

    let testMultipleConcatenations text_ctx =
      let cat1 = DequeTest.newDequeFrontFromList [1; 2] in
      let cat2 = DequeTest.newDequeFrontFromList [3; 4] in
      let cat3 = DequeTest.newDequeFrontFromList [5; 6] in
      let cat4 = DequeTest.newDequeFrontFromList [7; 8] in
      let cat5 = DequeTest.newDequeFrontFromList [9; 10] in
      let cat6 = DequeTest.newDequeFrontFromList [11; 12] in
      let cat7 = DequeTest.newDequeFrontFromList [13; 14] in
      let cat8 = DequeTest.newDequeFrontFromList [15; 16] in
      let cat12 = CatenableDeque.concat cat1 cat2 in
      let cat34 = CatenableDeque.concat cat3 cat4 in
      let cat56 = CatenableDeque.concat cat5 cat6 in
      let cat78 = CatenableDeque.concat cat7 cat8 in
      let cat1234 = CatenableDeque.concat cat12 cat34 in
      let cat5678 = CatenableDeque.concat cat56 cat78 in
      let result = CatenableDeque.concat cat1234 cat5678 in
      let () =
        List.iter
        (fun x -> print_int x; print_string " ")
        (DequeTest.toList result) in
      assert_equal
        ~msg:"Should concatenate deques properly"
        (DequeTest.toList result)
        [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16]

    let getTests =
      let catenableTests = [
        (* "testConcatenatingTwoDeques">:: testConcatenatingTwoDeques;
        "testOperationsOnCatenatedDeque">:: testOperationsOnCatenatedDeque; *)
        "testMultipleConcatenations">:: testMultipleConcatenations;
      ] in
      (* All tests to deques are applicable to catenable deques *)
      (* let dequeTests = DequeTest.getTests *)
      let dequeTests = []
      in List.append catenableTests dequeTests

    let run = run_test_tt_main ("suite">::: getTests)
end;;
