open Stream2;;
open IDeque;;

module Stream = Stream2;;

module RealTimeDeque: IDeque =
  struct
    type 'a t = {
      front: 'a stream;
      frontSize: int;
      frontSchedule: 'a stream;
      rear: 'a stream;
      rearSize: int;
      rearSchedule: 'a stream;
    }

    let factor = 2;;

    exception Empty_queue;;
    exception Invariant;;

    let newEmpty = {
      front = Stream2.empty;
      frontSize = 0;
      frontSchedule = Stream2.empty;
      rear = Stream2.empty;
      rearSize = 0;
      rearSchedule = Stream2.empty;
    }

    let isEmpty {frontSize; rearSize}: bool = frontSize + rearSize == 0;;

    let executeScheduleOnce (schedule: 'a stream): 'a stream
      = match schedule with
        | lazy (StreamCell (_, rest)) -> rest
        (* Empty schedule *)
        | schedule -> schedule
      ;;

    let executeScheduleTwice (schedule: 'a stream): 'a stream
     =  executeScheduleOnce (executeScheduleOnce schedule);;

    (*
      Because Stream.reverse is a monolithic operation, we change it to only
      execute 'factor' steps at a time. It transfer the elements of rear into
      front in reverse order, 'factor' elements at a time.
    *)
    let rec rotateAndReverse
      (dest: 'a stream) (src: 'a stream) (accumulator: 'a stream)
    : 'a stream = match dest with
      | lazy Nil -> Stream.concat (Stream.reverse src) accumulator
      | lazy (StreamCell (elem, restDest)) ->
        lazy (
          let (frontOfSrc, restOfSrc) = Stream.partitionAt factor src in
          (* Transfer 'factor' elements from the rear to accumulator in reverse
             order *)
          let newAccumulator =
            Stream.concat (Stream.reverse frontOfSrc) accumulator in
          StreamCell(
            elem,
            rotateAndReverse restDest restOfSrc newAccumulator
          )
        )
    ;;

    (*
      Because Stream.drop is a monolithic operation, we change it to only
      execute 'factor' steps at a time. It drops 'toDropCount' elements from
      src, 'factor' elements at a time. It then transfer the undropped elements
      to 'dest' in reverse order.

      TDB: How to decompose rotateAndDrop and rotateAndReverse such that we can
      call them independently?
    *)
    let rec lazyDropThenReverse
      (dest: 'a stream) (toDropCount: int) (src: 'a stream)
    : 'a stream =
      if toDropCount < factor then
        rotateAndReverse dest (Stream.drop toDropCount src) Stream.empty
      else match dest with
        | lazy (StreamCell (elem, restDest)) ->
          lazy (StreamCell(
            elem,
            lazyDropThenReverse
              restDest (toDropCount - factor) (Stream.drop factor src)
          ))
        | lazy Nil -> raise Invariant
    ;;

    (*
      Adjust structure to conform to invariants.
    *)
    let check {front; frontSize; frontSchedule; rear; rearSize; rearSchedule} =
      let dequeSize = frontSize + rearSize in
      if (frontSize > factor * rearSize + 1) then
        let newFrontSize = dequeSize / 2 in
        let newRearSize = dequeSize - newFrontSize in
        let newFront = Stream.take newFrontSize front in
        let newRear = lazyDropThenReverse rear newFrontSize front in
        {
          front = newFront;
          frontSize = newFrontSize;
          frontSchedule = newFront;
          rear = newRear;
          rearSize = newRearSize;
          rearSchedule = newRear;
        }
      else if (rearSize > factor * frontSize + 1) then
        let newRearSize = dequeSize / 2 in
        let newFrontSize = dequeSize - newRearSize in
        let newRear = Stream.take newRearSize rear in
        let newFront = lazyDropThenReverse front newRearSize rear in
        {
          front = newFront;
          frontSize = newFrontSize;
          frontSchedule = newFront;
          rear = newRear;
          rearSize = newRearSize;
          rearSchedule = newRear;
        }
      else
        {front; frontSize; frontSchedule; rear; rearSize; rearSchedule}
    ;;


    let peekFront {front; rear} = match (front, rear) with
      | (lazy Nil, lazy Nil) -> raise Empty_queue
      (* Due to the invariants, single_elem must be the only element in the
        deque *)
      | (lazy Nil, lazy (StreamCell(single_elem, _))) -> single_elem
      | (lazy (StreamCell(first_elem, _)), _) -> first_elem
    ;;

    (*
      It's possible to show that executing the schedule twice per insertion is
      enough to guarantee the streams are fully evaluated by the time they're
      needed.
    *)
    let popFront
      {front; frontSize; frontSchedule; rear; rearSize; rearSchedule} =
      match (front, rear) with
        | (lazy Nil, lazy Nil) -> raise Empty_queue
        (* Due to the invariants, single_elem must be the only element in the
          deque *)
        | (lazy Nil, lazy (StreamCell(single_elem, _))) -> newEmpty
        | (lazy (StreamCell(_, rest)), rear) -> check {
          front = rest;
          frontSize = frontSize - 1;
          frontSchedule = executeScheduleTwice frontSchedule;
          rear;
          rearSize;
          rearSchedule = executeScheduleTwice rearSchedule;
        }
    ;;

    (*
      It's possible to show that executing the schedule once per insertion is
      enough to guarantee the streams are fully evaluated by the time they're
      needed.
    *)
    let pushBack
      elem
      {front; frontSize; frontSchedule; rear; rearSize; rearSchedule} =
      check {
        front;
        frontSize;
        frontSchedule = executeScheduleOnce frontSchedule;
        rear = lazy (StreamCell (elem, rear));
        rearSize = rearSize + 1;
        rearSchedule = executeScheduleOnce rearSchedule;
      }

    (*
      Reverse the deque to make it easier to implement the reverse versions of
      peekFront, popFront and pushBack
    *)
    let reverse {front; frontSize; frontSchedule; rear; rearSize; rearSchedule} =
      {
        front = rear;
        frontSize = rearSize;
        frontSchedule = rearSchedule;
        rear = front;
        rearSize = frontSize;
        rearSchedule = frontSchedule;
      }

    (*
      These operations are the mirror of the regular queue ones, so we can flip
      the deque, perform the reverse operation and flip it back.
    *)
    let peekBack deque = reverse deque |> peekFront;;
    let popBack deque = reverse deque |> popFront |> reverse;;
    let pushFront elem deque = reverse deque |> pushBack elem |> reverse;;

  end
;;
