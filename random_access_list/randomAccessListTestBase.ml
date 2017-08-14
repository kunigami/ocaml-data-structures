open OUnit2;;
open IRandomAccessList;;

module MakeTest(RandomAccessList: IRandomAccessList) =
  struct
    let rec toList randomAccessList =
      if RandomAccessList.isEmpty randomAccessList
        then []
        else
          let head = RandomAccessList.head randomAccessList in
          let tail = RandomAccessList.tail randomAccessList in
          head :: (toList tail)
    ;;

    let testEmpty test_ctx =
      let emptyList = RandomAccessList.empty in
      assert_bool
        "Should return isEmpty for an empty list"
        (RandomAccessList.isEmpty emptyList)
    ;;

    let testSingletonAccess test_ctx =
      let sampleList = RandomAccessList.(
        empty |>
        push 10
      ) in
      assert_equal
        ~msg:"First element should be 10"
        (RandomAccessList.lookup 0 sampleList) 10
    ;;

    let testSimpleAccess test_ctx =
      let sampleList = RandomAccessList.(
        empty |>
        push 10 |>
        push 20 |>
        push 30
      ) in
      let () = assert_equal
        ~msg:"First element should be 30"
        (RandomAccessList.lookup 0 sampleList) 30
      in
      let () = assert_equal
        ~msg:"Second element should be 20"
        (RandomAccessList.lookup 1 sampleList) 20
      in
      assert_equal
        ~msg:"Third element should be 10"
        (RandomAccessList.lookup 2 sampleList) 10
    ;;

    let testHead test_ctx =
      let sampleList = RandomAccessList.(
        empty |>
        push 10 |>
        push 20
      ) in
      assert_equal
        ~msg:"The first element should be 20"
        (RandomAccessList.head sampleList) 20
    ;;

    let testConvertingRandomAccessListToRegularList test_ctx =
      let sampleList = RandomAccessList.(
        empty |>
        push 10 |>
        push 20 |>
        tail |>
        push 21 |>
        push 22
      ) in
      assert_equal
        ~msg:"It should convert a random access list properly"
        (toList sampleList) [22; 21; 10]

    let testUpdatingList test_ctx =
      let sampleList = RandomAccessList.(
        empty |>
        push 10 |>
        push 20 |>
        push 30 |>
        update 2 11
      ) in
      assert_equal
        ~msg:"It should update position 2 of the list with 11"
        (toList sampleList) [30; 20; 11]

    let suite =
      "suite">:::
       [
        "testEmpty">:: testEmpty;
        "testSingletonAccess">:: testSingletonAccess;
        "testSimpleAccess">:: testSimpleAccess;
        "testHead">:: testHead;
        "testConvertingRandomAccessListToRegularList">::
          testConvertingRandomAccessListToRegularList;
        "testUpdatingList">:: testUpdatingList;
       ]
    ;;

    let run = run_test_tt_main suite
  end
;;
