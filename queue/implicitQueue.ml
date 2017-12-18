(*

  Implementation of a queue using implicit recursive slowdown.

  We'll wrap elements in one of three types of digits: Zero, One and Two,
  containing a corresponding number of elements.

  A 'a queue can be either a shallow or a deep node. A shallow node is a
  terminal node containing a Zero or One.

  A deep node is composed of three parts:
    * Front - can be One or Two
    * Middle - a lazily evaluated queue with type ('a, 'a)
    * Rear - can be Zero or One
*)
open IQueue
open Exceptions

module ImplicitQueue: IQueue =
  struct

  type 'a digit = Zero | One of 'a | Two of 'a * 'a
  type 'a queue =
    Shallow of 'a digit |
    Deep of {
      front: 'a digit;
      middle: (('a * 'a) queue) Lazy.t;
      rear: 'a digit;
    }
  type 'a t = 'a queue

  let newEmpty = Shallow Zero

  let isEmpty queue = match queue with
    | Shallow Zero -> true
    | _ -> false

  (*
    We always add elements to the rear of the queue, unless the queue is a
    shallow node. See the four cases below.

    Note this requires non-uniform polymorphic recursion.
  *)
  let rec push: 'a. 'a -> 'a queue -> 'a queue =
    fun newElem queue -> match queue with
    (* Shallow nodes can contain one element, so this is trivial *)
    | Shallow Zero -> Shallow (One newElem)
    (* We need to add a deep node to hold the new element. Since the middle
      queue has to have pairs of elements, and the front of a deep node has to
      have at least one element, the only configuration possible is to have the
      two elements at the front *)
    | Shallow (One singleElem) ->
        Deep {
          front = Two (singleElem, newElem);
          middle = lazy newEmpty;
          rear = Zero;
        }
    (* We always insert at the rear of a deep node. If it's Zero, there's room
       for the new element *)
    | Deep {front; middle; rear = Zero;} ->
        Deep {front; middle; rear = One newElem;}
    (* We always insert at the rear of a deep node. If it's One, we need to
       overflow to the recursive queue. Since its elements are pairs, we need
       to push both elements and empty the rear.

       Note: even though the recursive push can propagate, the cost is O(1)
       because of lazy evaluation.
    *)
    | Deep {front; middle; rear = One rearElem;} ->
        Deep {
          front;
          middle = lazy (push (rearElem, newElem) (Lazy.force middle));
          rear = Zero;
        }

  (*
    We always remove elements from the fron of the queue, unless the queue is a
    shallow node. See the four cases below.

    Note this requires non-uniform polymorphic recursion.
  *)
  let rec pop: 'a. 'a queue -> 'a queue =
    fun queue -> match queue with
    (* No element to be removed *)
    | Shallow Zero -> raise Exceptions.Empty_queue
    (* Single-element of a shallow node *)
    | Shallow (One _) -> Shallow Zero
    (* Two-elements in the front *)
    | Deep {front = Two (elem1, elem2); middle; rear;} ->
        Deep {front = One (elem2); middle; rear;}
    (* One element from the front. We need to 'borrow' one element from the
      next queue. Since that queue works with pairs of elements, we'll replace
      the front with two elements.

      Note: the borrowing can propagate and because it has to evaluate a lazy
      expression, in the worst case it can result in an O(n) operation. But it
      can be shown the amortized cost is (1).
    *)
    | Deep {front = One (elem); middle; rear} ->
        let forcedMiddle = Lazy.force middle in
        if isEmpty forcedMiddle then Shallow rear
        else
          let (elem1, elem2) = peek forcedMiddle in
          Deep {
            front = Two (elem1, elem2);
            middle = lazy (pop forcedMiddle);
            rear;
          }

  let peek queue = match queue with
    | Shallow Zero -> raise Exceptions.Empty_queue
    | Shallow (One elem) -> elem
    | Deep {front = One elem;} -> elem
    | Deep {front = Two (elem, _);} -> elem

end;;
