open Queue_stream;;
open Test_base;;

module StreamQueueTest = Test_base.MakeTest(Queue_stream);;

let () =
  StreamQueueTest.run
;;
