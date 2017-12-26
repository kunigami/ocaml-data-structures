open ICatenableDeque
open IDeque

(*
  This catenable deque is a beefed up version of SimpleCatenableDeque to achive
  O(1) for concat. As the other, this can be either shallow or deep. The shallow
  forms the base of the recursive structure and it's a regular deque.

  The deep version has 5 parts. front, frontAux, middle, rearAux and rear.
  front, middle and rear are regular deques. The invariant is that front and
  rear have at least 3 elements and middle at least 2.

  frontAux and rearAux are the recursive structures. Instead of being catenable
  deques of deques like 'middle' was for SimpleCatenableDeque, they have an extra layer,
  called compound. The compound is pretty much the same as a
  SimpleCatenableDeque.catDeque except that 'middle' is a ImplicitCatenableDeque
  of deques, not SimpleCatenableDeque. The invariant is that the deques in a
  compount all have at least 2 elements.

  Some notes:

  1) All operations over a shallow catenable deque are the same as regular deque
  operations.

  2) Inserting at the front of a deep deque consists inserting at the front of
  the 'front' part of it.  Inserting at the rear is symetrically defined.

  3) Peeking either from front / rear are trivial given that a deep deque always
  has elements in the front / rear.

  4) A deque is considered too small if it has <4 elements. Small deques cannot
  be used as front/rear of deep catDeques.

  5) Concat never does not have recursive calls.

  6) popFront/popBack may call itself recursively, but it can be shown that the
     amortized cost is O(1). The intuition is that when the recursive call
     happens, front/rear gets big enough so what subsequent calls of
     popFront/popBack do not get recurse and the overall cost is amortized.
*)

module ImplicitCatenableDeque(Deque: IDeque): ICatenableDeque =
  struct

    type 'a catDeque =
      Shallow of 'a Deque.t |
      Deep of {
        front: 'a Deque.t;
        frontAux: ('a compound catDeque) Lazy.t;
        middle: 'a Deque.t;
        rearAux: ('a compound catDeque) Lazy.t;
        rear: 'a Deque.t;
      }
    and 'a compound =
      Simple of 'a Deque.t |
      Compound of {
        front: 'a Deque.t;
        middle: ('a compound catDeque) Lazy.t;
        rear: 'a Deque.t;
      }

    type 'a t = 'a catDeque

    (*
      -------------------------------------------------------------------------
      Util methods for manipulating deques.
      -------------------------------------------------------------------------
    *)

    (* Whether a deque is smaller than a given size. O(size) *)
    let rec isSmallerThan deque size =
      if size = 0 then false
      else if Deque.isEmpty deque then true
      else isSmallerThan (Deque.popFront deque) (size - 1)

    (* Instead of computing size directly so guarantee code runs in O(1) *)
    let tooSmall deque = isSmallerThan deque 4

    (* concat in O(|smallDeque|) *)
    let rec concatSmallLeft smallDeque deque =
      if Deque.isEmpty smallDeque then deque
      else concatSmallLeft
        (Deque.popBack smallDeque)
        (Deque.pushFront (Deque.peekBack smallDeque) deque)

    (* concat in O(|smallDeque|) *)
    let rec concatSmallRight deque smallDeque =
      if Deque.isEmpty smallDeque then deque
      else concatSmallRight
        (Deque.pushBack (Deque.peekFront smallDeque) deque)
        (Deque.popFront smallDeque)

    (*
      (front, rear) -> (front', middle, rear'), where middle
      contains the last item of front and the first of rear
    *)
    let share front rear =
      let middle = Deque.pushFront
        (Deque.peekBack front)
        (Deque.pushBack (Deque.peekFront rear) Deque.newEmpty) in
      (Deque.popBack front, middle, Deque.popFront rear)

    let dequeReplaceHead element deque =
      Deque.pushFront element (Deque.popFront deque)

    let dequeReplaceLast element deque =
      Deque.pushBack element (Deque.popBack deque)

    (*
      -------------------------------------------------------------------------
      Util methods for manipulating catDeque records.
      -------------------------------------------------------------------------
    *)

    let updateFront: 'a. ('a Deque.t -> 'a Deque.t) -> 'a catDeque -> 'a catDeque =
      fun frontUpdater catDeque -> match catDeque with
      | Shallow deque -> Shallow (frontUpdater deque)
      | Deep {front; frontAux; middle; rearAux; rear} ->
        Deep {front = frontUpdater front; frontAux; middle; rearAux; rear;}

    let updateRear: 'a. ('a Deque.t -> 'a Deque.t) -> 'a catDeque -> 'a catDeque =
      fun rearUpdater catDeque -> match catDeque with
      | Shallow deque -> Shallow (rearUpdater deque)
      | Deep {front; frontAux; middle; rearAux; rear} ->
        Deep {front; frontAux; middle; rearAux; rear = rearUpdater rear;}

    let lazyUpdateFrontAux: 'a. ('a compound catDeque -> 'a compound catDeque) -> 'a catDeque -> 'a catDeque =
      fun frontAuxUpdater catDeque -> match catDeque with
      | Deep {front; frontAux; middle; rearAux; rear} ->
        let newFrontAux = lazy (frontAuxUpdater (Lazy.force frontAux)) in
        Deep {front; frontAux = newFrontAux; middle; rearAux; rear}
      | _ -> raise (Invalid_argument "Shallow doesn't have frontAux")

    let lazyUpdateRearAux: 'a. ('a compound catDeque -> 'a compound catDeque) -> 'a catDeque -> 'a catDeque =
      fun rearAuxUpdater catDeque -> match catDeque with
      | Deep {front; frontAux; middle; rearAux; rear} ->
        let newRearAux = lazy (rearAuxUpdater (Lazy.force rearAux)) in
        Deep {front; frontAux; middle; rearAux = newRearAux; rear}
      | _ -> raise (Invalid_argument "Shallow doesn't have rearAux")

    let setMiddle newMiddle catDeque = match catDeque with
      | Deep {front; frontAux; middle; rearAux; rear} ->
        Deep {front; frontAux; middle = newMiddle; rearAux; rear}
      | _ -> raise (Invalid_argument "Shallow doesn't have middle")

    (*
      -------------------------------------------------------------------------
      Catenable deque methods.
      -------------------------------------------------------------------------
    *)

    let newEmpty = Shallow Deque.newEmpty

    let isEmpty catDeque = match catDeque with
      | Shallow deque -> Deque.isEmpty deque
      | _ -> false

    let pushFront element catDeque =
      updateFront (fun front -> Deque.pushFront element front) catDeque

    let peekFront catDeque = match catDeque with
      | Shallow deque -> Deque.peekFront deque
      | Deep {front} -> Deque.peekFront front

    let replaceHead element catDeque =
      updateFront (fun front -> dequeReplaceHead element front) catDeque

    (*
      pushBack, peekBack and popBack are symmetrical to their *front
      counterparts
    *)

    let pushBack element catDeque =
      updateRear (fun rear -> Deque.pushBack element rear) catDeque

    let peekBack catDeque = match catDeque with
      | Shallow deque -> Deque.peekBack deque
      | Deep {rear} -> Deque.peekBack rear

    let replaceLast element catDeque =
      updateRear (fun rear -> dequeReplaceLast element rear) catDeque

    let makeDeepFromShallows shallowFront shallowRear =
      let (front, middle, rear) = share shallowFront shallowRear in
        Deep {
          front; frontAux = lazy newEmpty; middle; rearAux = lazy newEmpty; rear;
        }

    let rec concat: 'a. 'a catDeque -> 'a catDeque -> 'a catDeque =
      fun catDeque1 catDeque2 -> match (catDeque1, catDeque2) with
        | (Shallow deque1, Shallow deque2) ->
          if tooSmall deque1 then Shallow (concatSmallLeft deque1 deque2)
          else if tooSmall deque2 then Shallow (concatSmallRight deque1 deque2)
          (* shallow deques are big enough to become front and rear.
             Middle has to have at least 2 elements, so we pick one from deque1
             and one from deque2 *)
          else makeDeepFromShallows deque1 deque2
        | (Shallow deque, Deep {front}) ->
          if tooSmall deque then
            updateFront (fun front -> concatSmallLeft deque front) catDeque2
          (* deque is big enough to be the new front. Push the old front
             into the auxiliary *)
          else
            catDeque2 |>
            updateFront (fun front -> deque) |>
            lazyUpdateFrontAux (pushFront (Simple front))
        | (Deep {rear}, Shallow deque) ->
          if tooSmall deque then
            updateRear (fun rear -> concatSmallRight rear deque) catDeque1
          (* deque is big enough to be the new rear. Push the old rear
             into the auxiliary *)
          else
            catDeque1 |>
            updateRear (fun rear -> deque) |>
            lazyUpdateRearAux (pushBack (Simple rear))
        | (
            Deep {
              front = front1;
              frontAux = frontAux1;
              middle = middle1;
              rearAux = rearAux1;
              rear = rear1;
            },
            Deep {
              front = front2;
              frontAux = frontAux2;
              middle = middle2;
              rearAux = rearAux2;
              rear = rear2;
            }
          ) ->
            (*
              To combine two deep cdeques we add a composed element to the

              F1, [FA1], M1, [RA1], R1
              F2, [FA2], M2, [RA2], R2

              M = R1[-1] + F2[0]
              R1' = R1 without the last item
              F2' = F2 without the first item

              The order of the combined "segment" is the same:

              F1, [FA1, [M1, RA1, R1']], M, [[F2', FA2, M2], RA2], R2
            *)
            let (rear1Init, middle, front2Tail) = share rear1 front2 in
            let newFrontAux = lazy (
              pushBack
                (Compound {front = middle1; middle = rearAux1; rear = rear1Init;})
                (Lazy.force frontAux1)
            ) in
            let newRearAux = lazy (
              pushFront
                (Compound {front = front2Tail; middle = frontAux2; rear = middle2;})
                (Lazy.force rearAux2)
            ) in
          Deep {
            front = front1;
            frontAux = newFrontAux;
            middle;
            rearAux = newRearAux;
            rear = rear2;
          }

      let rec popFront: 'a. 'a catDeque -> 'a catDeque =
        fun catDeque -> match catDeque with
        | Shallow deque -> Shallow (Deque.popFront deque)
        | Deep {front; frontAux; middle; rearAux; rear} ->
          (* front is big enough to maintain the invariant even if we remove
            an element *)
          if not (tooSmall front) then updateFront Deque.popFront catDeque
          else if not (isEmpty (Lazy.force frontAux)) then
            (* Borrowing from frontAux. *)
            match (peekFront (Lazy.force frontAux)) with
              (* 'Merge' the front with the first element of frontAux. Since
                 it has at least 2 elements, the resulting front is valid, even
                 after removing an element.
               *)
              | Simple deque ->
                catDeque |>
                updateFront
                  (fun front -> concatSmallLeft (Deque.popFront front) deque) |>
                (* Note the recursive call *)
                lazyUpdateFrontAux popFront
              | Compound {front = innerFront; middle = innerMiddle; rear = innerRear} ->
                catDeque |>
                updateFront
                  (fun front -> concatSmallLeft (Deque.popFront front) innerFront) |>
                (*
                  The main complexity here is updating frontAux. It "breaks
                  down" the compound 3 pieces. The front became part of the
                  cdeque front. The middle got merged with frontAux and the rear
                  replaced the first element of frontAux.
                *)
                lazyUpdateFrontAux
                  (fun frontAux ->
                    concat (Lazy.force innerMiddle) (replaceHead (Simple innerRear) frontAux)
                  )
          (* frontAux is empty *)
          else if not (isEmpty (Lazy.force rearAux)) then
            match (peekFront (Lazy.force rearAux)) with
              (* It's analogous to the case above, except that we borrow from
                middle which then borrows from rearAux. Middle has at least
                2 elements *)
              | Simple deque ->
                catDeque |>
                updateFront
                  (fun front -> concatSmallLeft (Deque.popFront front) middle) |>
                setMiddle deque |>
                (* Note the recursive call *)
                lazyUpdateRearAux popFront
              (* In the composed case, front borrows from middle. iinnerFront and
                 innerMiddle become the newFrontAux, while innerRear becomes
                 the middle.
              *)
              | Compound {front = innerFront; middle = innerMiddle; rear = innerRear} ->
                catDeque |>
                updateFront
                  (fun front -> concatSmallLeft (Deque.popFront front) middle) |>
                lazyUpdateFrontAux (fun frontAux ->
                  pushFront (Simple innerFront) (Lazy.force innerMiddle)
                ) |>
                setMiddle innerRear |>
                lazyUpdateRearAux popFront
          (* frontAux and rearAux are empty. Depending on the size of middle
             and rear, we can still end up with enough elements for a Deep
             cdeque. We can rely on concat to handle this for us *)
          else concat
            (Shallow (concatSmallLeft (Deque.popFront front) middle))
            (Shallow rear)

      let rec popBack: 'a. 'a catDeque -> 'a catDeque =
        fun catDeque -> match catDeque with
        | Shallow deque -> Shallow (Deque.popBack deque)
        | Deep {front; frontAux; middle; rearAux; rear} ->
          if not (tooSmall rear) then
            updateRear Deque.popBack catDeque
          else if not (isEmpty (Lazy.force rearAux)) then
            match (peekBack (Lazy.force rearAux)) with
              | Simple deque ->
                catDeque |>
                updateRear (fun rear -> concatSmallRight (Deque.popBack rear) deque) |>
                lazyUpdateRearAux popBack
              | Compound {front = innerFront; middle = innerMiddle; rear = innerRear} ->
                catDeque |>
                updateRear (fun rear -> concatSmallRight (Deque.popBack rear) innerRear) |>
                lazyUpdateRearAux (fun rearAux ->
                  concat (replaceLast (Simple innerFront) rearAux) (Lazy.force innerMiddle)
                )
          else if not (isEmpty (Lazy.force frontAux)) then
            match (peekBack (Lazy.force frontAux)) with
              | Simple deque ->
                catDeque |>
                lazyUpdateFrontAux popBack |>
                setMiddle deque |>
                updateRear
                  (fun rear -> concatSmallRight middle (Deque.popBack rear))
              | Compound {front = innerFront; middle = innerMiddle; rear = innerRear} ->
                catDeque |>
                lazyUpdateFrontAux popBack |>
                setMiddle innerFront |>
                lazyUpdateRearAux (fun rearAux ->
                  pushBack (Simple innerRear) (Lazy.force innerMiddle)
                ) |>
                updateRear
                  (fun rear -> concatSmallRight middle (Deque.popBack rear))
          (* rearAux and frontAux are empty *)
          else concat
            (Shallow front)
            (Shallow (concatSmallRight middle (Deque.popBack rear)))

  end
