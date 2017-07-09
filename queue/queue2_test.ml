open Queue2;;
open Test_base;;

module Queue2Test = Test_base.MakeTest(Queue2);;

let () =
  Queue2Test.run
;;
