open Queue_suspended;;
open IQueue;;
open Test_base;;

let queueModule = (module Queue_suspended: IQueue);;

let () =
  runTests queueModule
;;
