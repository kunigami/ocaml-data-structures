open DequeTestBase;;
open BankersDeque;;

module BankerDequeTest = MakeTest(BankersDeque);;

let () =
  BankerDequeTest.run
;;
