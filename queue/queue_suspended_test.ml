open Queue_suspended;;
open Test_base;;

module SuspendedQueueTest = Test_base.MakeTest(Queue_suspended);;

let () =
  SuspendedQueueTest.run
;;
