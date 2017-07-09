open Real_time_queue;;
open Test_base;;

module RealTimeQueueTest = Test_base.MakeTest(Real_time_queue);;

let () =
  RealTimeQueueTest.run
;;
