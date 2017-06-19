open Queue2;;
open IQueue;;
open Test_base;;

let queueModule = (module Queue2: IQueue);;

let () =
  runTests queueModule
;;
