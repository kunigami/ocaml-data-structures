open CatenableDequeBaseTest
open SimpleCatenableDeque
open BankersDeque

module SimpleCatenableDequeImpl = SimpleCatenableDeque(BankersDeque)
module SimpleCatenableDequeTest = MakeTest(SimpleCatenableDequeImpl)

let () =
  SimpleCatenableDequeTest.run
;;
