open Real_time_queue;;
open IQueue;;
open Test_base;;

let queueModule = (module Real_time_queue: IQueue);;

let () =
  runTests queueModule
;;
