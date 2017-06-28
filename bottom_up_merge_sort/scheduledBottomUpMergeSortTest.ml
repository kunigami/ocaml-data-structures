open BatInt;;
open ScheduledBottomUpMergeSort;;
open IBottomUpMergeSort;;
open OUnit2;;

module type IntBottomUpMergeSort = IBottomUpMergeSort with type t = int;;

module MakeTest(BottomUpMergeSort: IntBottomUpMergeSort) =
  struct

    let testSorting test_ctx =
      let sortedList = BottomUpMergeSort.(
        empty |>
        addElement 3 |>
        addElement 2 |>
        addElement 4 |>
        addElement 1 |>
        sort
      ) in
      assert_equal sortedList [1; 2; 3; 4];;
    ;;

    let suite =
    "suite">:::
     [
      "testSorting">:: testSorting;
     ]
    ;;

    let run = run_test_tt_main suite
    ;;

end;;

module IntScheduledBottomUpMergeSort = ScheduledBottomUpMergeSort(BatInt);;
module ScheduledBottomUpMergeSortTest =
  MakeTest(IntScheduledBottomUpMergeSort);;

let () =
  ScheduledBottomUpMergeSortTest.run
;;
