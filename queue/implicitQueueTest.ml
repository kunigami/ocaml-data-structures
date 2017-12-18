open ImplicitQueue;;
open Test_base;;

module ImplicitQueueTest = Test_base.MakeTest(ImplicitQueue);;

let () =
  ImplicitQueueTest.run
;;
