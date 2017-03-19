(*

  Implementation of a queue with two streams. This allows lazy operations for
  which we can apply amortized analysis.

  The first list represents the front
  of the queue and the second the end, reversed. Insertions are performed at the
  beginning of the second queue (remember inserting at the beginning of the list
  is more efficient). Removals are done at the front list.

  The invariant we keep is that 'front' is always greater than 'rear'.
*)

open Stream2;;

(*
  - size of front stream
  - stream representing the front of the queue
  - size of rear stream
  - stream representing the (reversed) rear of the queue
*)
type 'a queueStream = int * 'a stream * int * 'a stream;;

exception Empty_queue
;;

let newEmpty = (0, Stream2.empty, 0, Stream2.empty);;

let check (queue: 'a queueStream): ('a queueStream) = match queue with
  (frontSize, front, rearSize, rear) ->
    if rearSize <= frontSize then queue
    else (frontSize + rearSize, Stream2.concat front (Stream2.reverse rear), 0, Stream2.empty)
;;

let push (elem: 'a) (queue: 'a queueStream): ('a queueStream) = match queue with
  (frontSize, front, rearSize, rear) ->
    check (frontSize, front, rearSize + 1, Stream2.insert elem rear)
;;

let pop (queue: 'a queueStream): ('a queueStream) = match queue with
  (frontSize, front, rearSize, rear) ->
    let forcedfront = Lazy.force front in
    match forcedfront with
      | Nil -> raise Empty_queue
      | StreamCell (_, rest) -> check (frontSize - 1, rest, rearSize, rear)
;;

let peek (queue: 'a queueStream): 'a = match queue with
  (frontSize, front, rearSize, rear) ->
    let forcedfront = Lazy.force front in
    match forcedfront with
      | Nil -> raise Empty_queue
      | StreamCell (elem, _) -> elem
;;

let isEmpty (queue: 'a queueStream): bool = match queue with
  (* Assuming the invariant frontSize >= rearSize holds, we just need to check
    the front size *)
  (frontSize, _, _, _) -> frontSize == 0
;;
