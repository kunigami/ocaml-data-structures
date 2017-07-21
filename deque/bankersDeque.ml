open Stream2;;
open IDeque;;

module Stream = Stream2;;

(*
  In this structure, we maintain the invariants

  (1) frontSize <= factor * rearSize + 1 and
  (2) rearSize <= factor * frontSize + 1

  for factor > 2
*)
module BankersDeque: IDeque =
  struct
    type 'a t = {
      front: 'a stream;
      frontSize: int;
      rear: 'a stream;
      rearSize: int;
    }

    let factor = 2;;

    exception Empty_queue;;

    let newEmpty = {
      front = Stream.empty;
      frontSize = 0;
      rear = Stream.empty;
      rearSize = 0;
    }

    let isEmpty {frontSize; rearSize}: bool =
      frontSize + rearSize == 0
    ;;

    (*
      'Rebalance' front and rear such that the invariants (1) and (2) are
      maintained.

      It basically transfer elements from one stream to annother to restore the
      balance. It's possible to show that re-balancing yields an O(1) amortized
      insertion/removal operations.
    *)
    let check {front; frontSize; rear; rearSize} =
      let dequeSize = frontSize + rearSize in
      if frontSize > factor * rearSize + 1 then
        let newFrontSize = dequeSize / 2 in
        let newRearSize = dequeSize - newFrontSize in
        let (newFront, restFront) = Stream.partitionAt newFrontSize front in
        let newRear =
          Stream.concat rear (Stream.reverse restFront)
        in
        {
          front = newFront;
          frontSize = newFrontSize;
          rear = newRear;
          rearSize = newRearSize
        }
      else if rearSize > factor * frontSize + 1 then
        let newRearSize = dequeSize / 2 in
        let newFrontSize = dequeSize - newRearSize in
        let (newRear, restRear) = Stream.partitionAt newRearSize rear in
        let newFront =
          Stream.concat front (Stream.reverse restRear)
        in
        {
          front = newFront;
          frontSize = newFrontSize;
          rear = newRear;
          rearSize = newRearSize
        }
      else {front; frontSize; rear; rearSize}
    ;;

    let peekFront {front; rear} = match (front, rear) with
      | (lazy Nil, lazy Nil) -> raise Empty_queue
      (* Due to the invariants, single_elem must be the only element in the
        deque *)
      | (lazy Nil, lazy (StreamCell(single_elem, _))) -> single_elem
      | (lazy (StreamCell(first_elem, _)), _) -> first_elem
    ;;

    let popFront {front; frontSize; rear; rearSize} = match (front, rear) with
      | (lazy Nil, lazy Nil) -> raise Empty_queue
      | (lazy Nil, lazy (StreamCell(_, _))) -> newEmpty
      | (lazy (StreamCell(_, restFront)), rear) ->
        check {front = restFront; frontSize = frontSize - 1; rear; rearSize}
    ;;

    let pushBack elem {front; frontSize; rear; rearSize} =
      check {
        front;
        frontSize;
        rear = lazy (StreamCell (elem, rear));
        rearSize = rearSize + 1;
      }

    let reverse {front; frontSize; rear; rearSize} =
      {front = rear; frontSize = rearSize; rear = front; rearSize = frontSize}
    ;;

    (*
      These operations are the mirror of the regular queue ones, so we can flip
      the deque, perform the reverse operation and flip it back.
    *)
    let peekBack deque = reverse deque |> peekFront;;
    let popBack deque = reverse deque |> popFront |> reverse;;
    let pushFront elem deque = reverse deque |> pushBack elem |> reverse;;

  end
;;
