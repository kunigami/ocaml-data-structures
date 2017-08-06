open IQueue;;

(*
  The idea of this queue is to achieve worst-case bounds by partially executing
  expensive operations in a parallel structure. It does so without lazy
  evaluation. This separate structure is the rotationState.

  The invariant is that rear list should not be larger than front list. When
  that happens we start the process of transferring the elements from the rear
  to the front.

  The rotation operation consists in reversing rear and concatenating it to the
  end of front, that is front + ~rear. We cannot perform this operation
  one-by-one. The operation we can do partially is contatenating the reverse of
  a list to the begining of another list, that is, given xs and ys, we can do
  ~xs + ys. We can also do xs to ~xs partially by having ys = [].

  To achieve xs + ~ys, we reverse both xs and ys, then concatenate the reverse
  of ~xs to ~y

  1) (xs, ys)
  2) (~xs, ~ys)
  3) xs + ~ys

  Operation 2) is represented by the state 'Reversing'. Operation 3) is
  represented by the state 'Appending'.

  The problem of keeping a separate state is that it might not be accurate by
  the time it's done. In particular, if we remove an element from the front,
  step 3) can be shortcut. okCount represents how many of the elements in ~xs of
  step 2 are valid. If, by the time we're appending okCount goes to 0, the last
  elements of ~xs (that is, the first elements of xs), have been removed, so
  they do not need to be transferred to ~ys.
*)
module HoodMevilleQueue: IQueue =
  struct

  type 'a rotationState =
    Idle
    | Reversing of {
      okCount: int;
      front: 'a list;
      rear: 'a list;
      frontTemp: 'a list;
      rearTemp: 'a list;
    }
    | Appending of {
      okCount: int;
      front: 'a list;
      rear: 'a list;
    }
    | Done of 'a list
  ;;

  type 'a queue = {
    frontSize: int;
    front: 'a list;
    rearSize: int;
    rear: 'a list;
    rotation: 'a rotationState;
  }

  type 'a t = 'a queue;;

  exception Empty_queue;;

  let newEmpty = {
    frontSize = 0;
    front = [];
    rearSize = 0;
    rear = [];
    rotation = Idle;
  };;

  let isEmpty {frontSize} =
    frontSize == 0;;

  let nextState rotationStep =
    match rotationStep with
    | Reversing ({
        okCount;
        front = firstElem :: restFront;
        frontTemp;
        rear = lastElem :: restRear;
        rearTemp;
      }) -> Reversing ({
        okCount = okCount + 1;
        front = restFront;
        frontTemp = firstElem :: frontTemp;
        rear = restRear;
        rearTemp = lastElem :: rearTemp;
      })
    | Reversing ({
        okCount;
        front = [];
        frontTemp;
        rear = lastElem :: restRear; (* Invariant: restRear must be empty *)
        rearTemp;
      }) -> Appending ({
        okCount;
        front = frontTemp;
        rear = lastElem :: rearTemp;
      })
    | Appending ({okCount = 0; front; rear}) -> Done rear
    (* Transfer one element of front to rear *)
    | Appending ({okCount; front = elem :: restFront; rear}) ->
        Appending ({
          okCount = okCount - 1;
          front = restFront;
          rear = elem :: rear;
        })
    | rotationStep -> rotationStep (* No-op *)
  ;;

  let invalidate rotationStep = match rotationStep with
    | Reversing ({okCount; front; frontTemp; rear; rearTemp})
      -> Reversing ({
        okCount = okCount - 1;
        front;
        frontTemp;
        rear;
        rearTemp;
      })
    | Appending ({okCount = 0; front; rear = lastElem :: restRear})
      -> Done restRear
    | Appending ({okCount; front; rear}) ->
        Appending ({okCount = okCount - 1; front; rear})
    | rotationStep -> rotationStep
  ;;

  let processStateFromQueue {frontSize; front; rearSize; rear; rotation} =
    let newState = (nextState (nextState rotation)) in match newState with
      (* How do we guarantee newFront has the same size as front? *)
      | Done newFront ->
        {frontSize; front = newFront; rearSize; rear; rotation = Idle}
      | _ -> {frontSize; front; rearSize; rear; rotation = newState}
  ;;

  let check queue = match queue with
    {frontSize; front; rearSize; rear; rotation} ->
      if rearSize <= frontSize then processStateFromQueue queue
      else
        (* Initiate the rotation process. *)
        let newState = Reversing ({
          okCount = 0;
          front;
          frontTemp = [];
          rear;
          rearTemp = [];
        })
        in processStateFromQueue {
          frontSize = frontSize + rearSize;
          front;
          rearSize = 0;
          rear = [];
          rotation = newState;
        }
  ;;

  let push elem {frontSize; front; rearSize; rear; rotation} =
    check {
      frontSize;
      front;
      rearSize = rearSize + 1;
      rear = elem :: rear;
      rotation;
    }
  ;;

  let peek {frontSize; front; rearSize; rear; rotation} = match front with
    | [] -> raise Empty_queue
    | firstElem :: restFront -> firstElem
  ;;

  let pop {frontSize; front; rearSize; rear; rotation} = match front with
    | [] -> raise Empty_queue
    | firstElem :: restFront ->
      let newState = invalidate rotation in
      check {
        frontSize = frontSize - 1;
        front = restFront;
        rearSize;
        rear;
        rotation = newState;
      }
  ;;

  end;;
