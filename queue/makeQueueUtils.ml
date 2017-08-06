open IQueue;;

module MakeQueueUtils(Queue: IQueue) =
  struct
    let rec toList queue =
      if Queue.isEmpty queue then []
      else
        let elem = Queue.peek queue in
        elem :: (toList (Queue.pop queue))
    ;;
end;;
