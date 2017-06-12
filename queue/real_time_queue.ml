(*

  Implementation of a real time queue [1]

  The first list represents the front
  of the queue and the second the end, reversed. Insertions are performed at the
  beginning of the second queue (remember inserting at the beginning of the list
  is more efficient). Removals are done at the front list.

  The invariant we keep is that 'front' is always greater than 'rear'.

  [1] Robert Hood and Rober Melvill. Real-time queue operations in pure Lisp
*)

open Stream2;;

type 'a realTimeQueue = {
  front: 'a stream;
  rear: 'a list;
  schedule: 'a stream
}

exception Empty_queue
;;

let newEmpty = {
  front = Stream2.empty;
  rear = [];
  schedule = Stream2.empty
}

(* TODO: Change input parameter to record *)
let rec rotate (queue: 'a realTimeQueue) = match queue with { front; rear; schedule } ->
  let forced_front = Lazy.force front in
  match rear with
     (*
       This should never happen with a valid queue because rotate is only called when
       |rear| > |front|
      *)
    | [] -> raise Empty_queue
    | last_elem :: rest_rear ->
    match forced_front with
      | Nil -> Stream2.insert last_elem schedule
      | StreamCell (first_elem, rest_front) ->
          Stream2.insert
            first_elem
            (rotate {
              front = rest_front;
              rear = rest_rear;
              schedule = (Stream2.insert last_elem schedule)
            })
;;

(*
 * exec evaluates the first element of the schedule stream. Because of memoization, this means that
 * whenever we evaluate 'front',  we guarantee that all operations are already memoized.
 *)
let exec (queue: 'a realTimeQueue) = match queue with
  | { front; rear; schedule } ->
    let forced_schedule = Lazy.force schedule in match forced_schedule with
      | StreamCell (_, rest_schedule) -> { front; rear; schedule = rest_schedule }
      (* Due to invariants, this means that |rear| > |front|  *)
      | Nil ->
        let newFront = rotate {front; rear; schedule = Stream2.empty} in
        {front = newFront; rear = []; schedule = newFront}
;;

let push (elem: 'a) (queue: 'a realTimeQueue): ('a realTimeQueue) = match queue with
  { front; rear; schedule } -> exec { front; rear = elem :: rear ; schedule }
;;

let pop (queue: 'a realTimeQueue): 'a realTimeQueue = match queue with
  { front; rear; schedule } ->
    let forcedfront = Lazy.force front in
    match forcedfront with
      | Nil -> raise Empty_queue
      | StreamCell (_, rest_front) -> exec { front = rest_front ; rear ; schedule }
;;

let peek (queue: 'a realTimeQueue): 'a = match queue with
  { front; rear; schedule } ->
    let forcedfront = Lazy.force front in
    match forcedfront with
      | Nil -> raise Empty_queue
      | StreamCell (first_elem, _) -> first_elem
;;

let toList (queue: 'a realTimeQueue): 'a list = match queue with
  { front; rear; schedule } ->
    let frontList = Stream2.toList front in
    frontList @ (List.rev rear)
;;
