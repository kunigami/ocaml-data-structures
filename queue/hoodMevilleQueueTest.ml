open HoodMevilleQueue;;
open Test_base;;

module HoodMevilleQueueTest = Test_base.MakeTest(HoodMevilleQueue);;

let () =
  HoodMevilleQueueTest.run
;;
