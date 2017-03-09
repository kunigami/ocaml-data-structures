
(*
    Implemention of queue using lazy lists. As opposed to the stream-based
    queue, this structure allows for a monolithic amortization analysis, for
    example, using the Physicist's method as described in Okazaki's Purely
    Functional Data Structures, Chapter 6.

    Only the front list needs to be lazy. The back of the queue is a regular
    list. We store a copy of the suspended list to be able to access the head of
    the list without having to evaluate the entire list.

    The first element in the structure is the evaluated version of the front.
    The second represents the size of the front list. The third element is the
    suspended version of the front. The fourth element is the size of the back
    list and finally the fifth element is the back list.

    The invariant that must be respected by all operations is that the front
    list must never be smaller than the back list.
*)
type 'a queueSuspended = 'a list * int * ('a list) Lazy.t * int * 'a list;;

let newEmpty = ([], 0, lazy [], 0, [])

let isEmpty (queue: 'a queueSuspended): bool = match queue with
  (_, leftSize, _, _, _) -> leftSize == 0
;;
