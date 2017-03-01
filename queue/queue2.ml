(*

  Implementation of a queue with two lists. The first list represents the front
  of the queue and the second the end, reversed. Insertions are performed at the
  beginning of the second queue (remember inserting at the beginning of the list
  is more efficient). Removals are done at the front list.

  When the removal of an element will leave the front list empty, an operation
  is performed to reverse the second list and assign it to the first.

  The invariant we keep is that if 'left' is empty then 'right' must empty as
  well.

*)
type 'a queue = 'a list * 'a list

exception Empty_queue
;;

let newEmpty = ([], [])
;;

(* Returns the first element of the queue *)
let front (queue: 'a queue): 'a = match queue with
  | ([], _) -> raise Empty_queue
  | (x::xs, _) -> x
;;

(* Returns the queue without the first element. Also known as tail() *)
let rec pop (queue: 'a queue): ('a queue) = match queue with
  (* If 'left' is empty, 'right' must be empty due to the invariant *)
  | ([], _) -> ([], [])
  (*
    If 'left' will be empty after the removal, transfer elements from the
    right
  *)
  | ([x], right) -> (List.rev right, [])
  | (x::xs, right) -> (xs, right)
;;

(* Inserts an element at the end of the queue *)
let push (elem: 'a) (queue: 'a queue): ('a queue) = match queue with
  (*
    'right' must be empty due to the invariant. This is the only case in which
    insert the element in the left to keep the invariant.
  *)
  | ([], _) -> ([elem], [])
  | (left, right) -> (left, elem :: right)
;;

let append (queue: 'a queue) (otherQueue: 'a queue): ('a queue) = match (queue, otherQueue) with
  | ((leftA, rightA), (leftB, rightB)) -> (
      leftA,
      rightB @ (List.rev leftB) @ rightA
    )

(* Decides whether the queue is empty *)
let empty (queue: 'a queue): bool = match queue with
 | ([], _) -> true
 | _ -> false
;;
