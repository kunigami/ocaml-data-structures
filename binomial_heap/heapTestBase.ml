open BatPervasives;;
open OUnit2;;
open IHeap;;

module type IntBinomialHeap = IHeap with type tv = int;;

module MakeTest(BinomialHeap: IntBinomialHeap) =
  struct
    let rec toList (heap: BinomialHeap.heap): BinomialHeap.tv list =
      if (BinomialHeap.isEmpty heap) then []
      else
          let minElem = BinomialHeap.findMin heap in
          minElem :: (toList (BinomialHeap.deleteMin heap))
    ;;

    let rec fromList (ls: BinomialHeap.tv list): BinomialHeap.heap =
      List.fold_left
        (BatPervasives.flip BinomialHeap.insert)
        BinomialHeap.empty ls
    ;;

    let testSortingViaHeap test_ctx =
      let heap = fromList [5; 7; 3; 1; 10; 8] in
      let sortedList = toList heap in
      assert_equal sortedList [1; 3; 5; 7; 8; 10];;
    ;;

    let suite =
    "suite">:::
     [
      "testSortingViaHeap">:: testSortingViaHeap;
     ]
    ;;

    let run = run_test_tt_main suite
    ;;
end;;
