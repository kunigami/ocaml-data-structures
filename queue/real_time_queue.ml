(*

  Implementation of a real time queue [1]. Which is a persistent structure with worst-case O(1)
  insertion and removal .

  The first list represents the front of the queue and the second the end, reversed. Insertions are
  performed at the beginning of the second queue (remember inserting at the beginning of the list is
  more efficient). Removals are done at the front list.

  [1] Robert Hood and Rober Melvill. Real-time queue operations in pure Lisp
*)

open Stream2;;
open IQueue;;

module Stream = Stream2;;

module Real_time_queue: IQueue =
  struct
    type 'a realTimeQueue = {
      front: 'a stream;
      rear: 'a list;
      schedule: 'a stream
    }

    (* Fill in the type for the implementation *)
    type 'a t = 'a realTimeQueue;;

    exception Empty_queue;;

    let newEmpty = {
      front = Stream.empty;
      rear = [];
      schedule = Stream.empty
    }

    let rec rotate ({ front; rear; schedule }: 'a realTimeQueue) =
      match rear with
         (*
           This should never happen with a valid queue because rotate is only called when
           |rear| > |front|
          *)
        | [] -> raise Empty_queue
        | last_elem :: rest_rear ->
        match front with
          | lazy Nil -> Stream.insert last_elem schedule
          | lazy (StreamCell (first_elem, rest_front)) ->
              Stream.insert
                first_elem
                (rotate {
                  front = rest_front;
                  rear = rest_rear;
                  schedule = (Stream.insert last_elem schedule)
                })
    ;;

    (*
     * exec evaluates the first element of the schedule stream. Because of memoization, this means that
     * whenever we evaluate 'front',  we guarantee that all operations are already memoized.
     *)
    let exec ({ front; rear; schedule }: 'a realTimeQueue) =
      match schedule with
        | lazy (StreamCell (_, rest_schedule)) ->
          { front; rear; schedule = rest_schedule }
        (* Due to invariants, this means that |rear| > |front|  *)
        | lazy Nil ->
          let newFront = rotate {front; rear; schedule = Stream.empty} in
          {front = newFront; rear = []; schedule = newFront}
    ;;

    let push
      (elem: 'a)
      ({ front; rear; schedule }: 'a realTimeQueue):
    ('a realTimeQueue) =
      exec { front; rear = elem :: rear ; schedule }
    ;;

    let pop ({ front; rear; schedule }: 'a realTimeQueue): 'a realTimeQueue =
      match front with
        | lazy Nil -> raise Empty_queue
        | lazy (StreamCell (_, rest_front)) ->
          exec { front = rest_front ; rear ; schedule }
    ;;

    let peek ({ front; rear; schedule }: 'a realTimeQueue): 'a =
        match front with
          | lazy Nil -> raise Empty_queue
          | lazy (StreamCell (first_elem, _)) -> first_elem
    ;;

    let toList ({ front; rear; schedule }: 'a realTimeQueue): 'a list =
      let frontList = Stream.toList front in
      frontList @ (List.rev rear)
    ;;

end;;
