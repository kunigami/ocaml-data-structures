(*

  Implementation of a queue with two streams. This allows lazy operations for
  which we can apply amortized analysis.

  The first list represents the front
  of the queue and the second the end, reversed. Insertions are performed at the
  beginning of the second queue (remember inserting at the beginning of the list
  is more efficient). Removals are done at the front list.

  The invariant we keep is that 'left' is always greater than 'right'.
*)

open Stream2;;

(*
  - size of left stream
  - left stream representing the front of the queue
  - size of right stream
  - right stream representing the (reversed) end of the queue
*)
type 'a queueStream = int * 'a stream * int * 'a stream;;

exception Empty_queue
;;

let newEmpty = (0, Stream2.empty, 0, Stream2.empty);;

let check (queue: 'a queueStream): ('a queueStream) = match queue with
  (leftSize, left, rightSize, right) ->
    if rightSize <= leftSize then queue
    else (leftSize + rightSize, Stream2.concat left (Stream2.reverse right), 0, Stream2.empty)
;;

let push (elem: 'a) (queue: 'a queueStream): ('a queueStream) = match queue with
  (leftSize, left, rightSize, right) ->
    check (leftSize, left, rightSize + 1, Stream2.insert elem right)
;;

let pop (queue: 'a queueStream): ('a queueStream) = match queue with
  (leftSize, left, rightSize, right) ->
    let forcedLeft = Lazy.force left in
    match forcedLeft with
      | Nil -> raise Empty_queue
      | StreamCell (_, rest) -> check (leftSize - 1, rest, rightSize, right)
;;

let peek (queue: 'a queueStream): 'a = match queue with
  (leftSize, left, rightSize, right) ->
    let forcedLeft = Lazy.force left in
    match forcedLeft with
      | Nil -> raise Empty_queue
      | StreamCell (elem, _) -> elem
;;

let isEmpty (queue: 'a queueStream): bool = match queue with
  (* Assuming the invariant leftSize >= rightSize holds, we just need to check
    the left size *)
  (leftSize, _, _, _) -> leftSize == 0
;;
