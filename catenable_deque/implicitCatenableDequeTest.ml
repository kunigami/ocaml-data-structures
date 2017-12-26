open CatenableDequeBaseTest
open ImplicitCatenableDeque
open BankersDeque

module ImplicitCatenableDequeImpl = ImplicitCatenableDeque(BankersDeque)
module ImplicitCatenableDequeTest = MakeTest(ImplicitCatenableDequeImpl)

let () =
  ImplicitCatenableDequeTest.run
;;
