
(*
    Implemention of queue using lazy lists. As opposed to the stream-based
    queue, this structure allows for a monolithic amortization analysis, for
    example, using the Physicist's method as described in Okazaki's Purely
    Functional Data Structures, Chapter 6.

    Only the front list needs to be lazy. The rear of the queue is a regular
    list. We store a copy of the suspended list to be able to access the head of
    the list without having to evaluate the entire list.

    The first element in the structure is the evaluated version of the front
    (forcedFront). The second represents the size of the front list (frontSize).
    The third element is the suspended version of the front (lazyFront). The
    fourth element is the size of the rear list (rearSize) and finally the fifth
    element is the rear list (rear).

    The invariants that must be respected by all operations are:
    1) The front list must never be smaller than the rear list
    2) Whenever lazyFront is non-empty, forcedFront is non-empty
*)
type 'a queueSuspended = 'a list * int * ('a list) Lazy.t * int * 'a list;;

let newEmpty = ([], 0, lazy [], 0, [])

let isEmpty (queue: 'a queueSuspended): bool = match queue with
  (_, frontSize, _, _, _) -> frontSize == 0
;;

(* let push (elem: 'a) (queue: 'a queueSuspended): ('a queueSuspended) = match queue with
  (leftSize, left, rightSize, right) ->
    check (leftSize, left, rightSize + 1, Stream2.insert elem right)
;; *)

(*
  Makes sure the queue satisfies the invariant: If lazyFront is non-empty,
  forcedFront is non-empty
*)
let checkForcedFrontInvariant (queue: 'a queueSuspended): ('a queueSuspended) =
  match queue with
    | ([], frontSize, lazyFront, rearSize, rear) ->
      (Lazy.force lazyFront, frontSize, lazyFront, rearSize, rear)
    | queue -> queue
;;

(*
  Makes sure the queue satisfies the invariant: The front list must never be
  smaller than the rear list
*)
let checkInvariants (
  queue: 'a queueSuspended
): ('a queueSuspended) = match queue with
  (forcedFront, frontSize, lazyFront, rearSize, rear) ->
    if rearSize <= frontSize then checkForcedFrontInvariant queue
    (* Invariant violated: reverse rear and concatenate w/ frontLazy. Not that
    we do not update frontForced with the newest version of front *)
    else let front = Lazy.force lazyFront
      in checkForcedFrontInvariant (
        front,
        frontSize + rearSize,
        lazy (front @ (List.rev rear)),
        0,
        []
      )
;;

let push (queue: 'a queueSuspended) (elem: 'a): ('a queueSuspended) =
  match queue with (forcedFront, frontSize, lazyFront, rearSize, rear) ->
    checkInvariants (
      forcedFront,
      frontSize,
      lazyFront,
      rearSize + 1,
      elem :: rear
    )
