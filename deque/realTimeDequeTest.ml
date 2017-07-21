open DequeTestBase;;
open RealTimeDeque;;

module RealTimeDequeTest = MakeTest(RealTimeDeque);;

let () =
  RealTimeDequeTest.run
;;
