open ICatenableDeque
open IDeque

(*
  This catenable deque can be either shallow or deep. The shallow forms the base
  of the recursive structure and it's a regular deque.

  The deep version is composed of three parts: front, middle and rear.
  front and rear are regular deques (not necessarily catenable). Middle is
  defined recursively as a lazy catenable deque of deques. 'front' represent the
  first elements in the deque, 'rear' the last ones. All the other elements are
  in the middle. The invariant for a deep deque is that 'front' and 'rear' have
  each at least 2 elements.

  Some notes:

  1) All operations over a shallow catenable deque are the same as regular deque
  operations.

  2) Inserting at the front of a deep deque consists inserting at the front of
  the 'front' part of it.  Inserting at the rear is symetrically defined.

  3) Peeking either from front / rear are trivial given that a deep deque always
  has elements in the front / rear.

  4) Popping an element from the front is more involved because it might require
  "borrowing" elements from the middle to preserve the invariant from the front.
  Popping can turn a deep deque into a shallow one. Same applies for popping
  from the end.

  5) The only operation that can create deep deques is the concat operation.
  This happens when each of the deques have enough elements to be front and rear
  of a deep deque.

  6) We only insert an element (i.e. a deque) into the middle catenable deque
  when it has size >= 2. This means that are < n/2 elements (deques) in the
  middle catenable deque. This in turn means that the height of a catenable
  deque is log(n).

  7) concat may be called recursively up to min(log(n1), log(n2)) where n1 and
  n2 are the respective sizes of the catenable deques to concatenate. Because
  this forces the evaluation of middle, this is the amortized complexity of the
  concat function.

*)
module SimpleCatenableDeque(Deque: IDeque): ICatenableDeque =
  struct

    type 'a catDeque =
      Shallow of 'a Deque.t |
      Deep of {
        front: 'a Deque.t;
        middle: ('a Deque.t catDeque) Lazy.t;
        rear: 'a Deque.t;
      }

    let newEmpty = Shallow Deque.newEmpty

    type 'a t = 'a catDeque

    (*
      -------------------------------------------------------------------------
      Util methods for manipulating deques.
      -------------------------------------------------------------------------
    *)

    let tooSmall deque =
      Deque.isEmpty deque || Deque.isEmpty (Deque.popFront deque)

    (* Assumes smallDeque's size is <= 1 *)
    let concatSmallLeft smallDeque deque =
      if Deque.isEmpty smallDeque then deque
      else Deque.pushFront (Deque.peekFront smallDeque) deque

    (* Assumes smallDeque's size is <= 1 *)
    let concatSmallRight deque smallDeque =
      if Deque.isEmpty smallDeque then deque
      else Deque.pushBack (Deque.peekFront smallDeque) deque

    (*
      -------------------------------------------------------------------------
      Catenable queue's methods
      -------------------------------------------------------------------------
    *)

    let isEmpty catDeque = match catDeque with
      | Shallow deque -> Deque.isEmpty deque
      | _ -> false

    let pushFront element catDeque = match catDeque with
      | Shallow deque -> Shallow (Deque.pushFront element deque)
      | Deep {front; middle; rear} ->
        Deep {front = Deque.pushFront element front; middle; rear}

    let peekFront catDeque = match catDeque with
      | Shallow deque -> Deque.peekFront deque
      | Deep {front; middle; rear} -> Deque.peekFront front

    let rec popFront: 'a. 'a catDeque -> 'a catDeque =
      fun catDeque -> match catDeque with
      | Shallow deque -> Shallow (Deque.popFront deque)
      | Deep {front; middle; rear} ->
        let newFront = Deque.popFront front in
        (* deque has size > 1 *)
        if not(tooSmall newFront) then Deep {front = newFront; middle; rear}
        else if isEmpty (Lazy.force middle) then
          Shallow (concatSmallLeft newFront rear)
        (* newFront is either empty or has a single element *)
        else Deep {
          front = concatSmallLeft newFront (peekFront (Lazy.force middle));
          middle = lazy (popFront (Lazy.force middle));
          rear;
        }

    (*
      pushBack, peekBack and popBack are symmetrical to their *front
      counterparts
    *)

    let pushBack element catDeque = match catDeque with
      | Shallow deque -> Shallow (Deque.pushBack element deque)
      | Deep {front; middle; rear} ->
        Deep {front; middle; rear = Deque.pushBack element rear}

    let peekBack catDeque = match catDeque with
      | Shallow deque -> Deque.peekBack deque
      | Deep {front; middle; rear} -> Deque.peekBack rear

    let rec popBack: 'a. 'a catDeque -> 'a catDeque =
      fun catDeque -> match catDeque with
      | Shallow deque -> Shallow (Deque.popBack deque)
      | Deep {front; middle; rear} ->
        let newRear = Deque.popBack rear in
        if not(tooSmall newRear) then Deep {front; middle; rear = newRear}
        else if isEmpty (Lazy.force middle) then
          Shallow (concatSmallRight front newRear)
        else Deep {
          front;
          middle = lazy (popFront (Lazy.force middle));
          rear = concatSmallRight (peekBack (Lazy.force middle)) newRear;
        }

    let rec concat: 'a. 'a catDeque -> 'a catDeque -> 'a catDeque =
      fun catDeque1 catDeque2 -> match (catDeque1, catDeque2) with
      | (Shallow deque1, Shallow deque2) ->
        if tooSmall deque1 then
          Shallow (concatSmallLeft deque1 deque2)
        else if tooSmall deque2 then
          Shallow (concatSmallRight deque1 deque2)
        (*
          Both deque1 and deque2 are big enough to become the front and end of
          a deep queue.
         *)
        else Deep {
          front = deque1;
          middle = lazy newEmpty;
          rear = deque2;
        }
      | (Shallow deque, Deep {front; middle; rear}) ->
        if tooSmall deque then
          Deep {
            front = concatSmallLeft deque front;
            middle;
            rear;
          }
        else
          Deep {
            front = deque;
            middle = lazy (pushFront front (Lazy.force middle));
            rear;
          }
      | (Deep {front; middle; rear}, Shallow deque) ->
        if tooSmall deque then
          Deep {
            front;
            middle;
            rear = concatSmallRight rear deque;
          }
        else
          Deep {
            front;
            middle = lazy (pushBack rear (Lazy.force middle));
            rear = deque;
          }
      | (
          Deep {front = front1; middle = middle1; rear = rear1},
          Deep {front = front2; middle = middle2; rear = rear2}
        ) ->
        (*
          Evaluating this can cascade and cause the evaluation of all middle
          queues recursively. This can be lead to O(min(log(n1), log(n2)))
          calls.
         *)
        let newMiddle = lazy (
          let combinedDeque1 = pushBack rear1 (Lazy.force middle1) in
          let combinedDeque2 = pushFront front2 (Lazy.force middle2) in
          concat combinedDeque1 combinedDeque2
        ) in
        Deep {
          front = front1;
          middle = newMiddle;
          rear = rear2;
        }
  end
