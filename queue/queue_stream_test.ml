open Queue_stream;;
open IQueue;;
open Test_base;;

let queueModule = (module Queue_stream: IQueue);;

let () =
  runTests queueModule
;;
