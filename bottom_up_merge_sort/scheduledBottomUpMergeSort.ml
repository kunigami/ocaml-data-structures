open Stream2;;
open IBottomUpMergeSort;;

module Stream = Stream2;;

(*
  Imagine we want to sort several lists, that share a large tails. For example,
  xs and x::xs or xs @ zs and ys @ zs. We want to take advantage of this common
  tail.

  Bottom-up merge sort splits a list into n segments, each with a single
  element. In phase 1, it then merges equal sizes segments, until only one
  element of each size remains. In phase 2, these are merged, from smallest to
  largest.

  At the end of phase 1 we can represent the segments by the digits of the
  binary representation of n. That's the invariant this structure works with.
  Adding an element to this, is analogous to incrementing a binary number by 1,
  where merges are the carry over.
*)

module ScheduledBottomUpMergeSort
  (Element: Set.OrderedType)
: IBottomUpMergeSort with type t = Element.t =
  struct
    type t = Element.t;;
    type schedule = t stream list;;
    type segment = {
      data: t stream;
      schedule: schedule;
    };;
    type sortable = {
      size: int;
      segments: segment list;
    };;

    (* Merge 2 sorted streams into a sorted stream *)
    let rec mergeStream (streamA: t stream) (streamB: t stream): t stream =
      let forcedStreamA = Lazy.force streamA in
      let forcedStreamB = Lazy.force streamB in
      match (forcedStreamA, forcedStreamB) with
        | (Nil, _) -> streamB
        | (_, Nil) -> streamA
        | (StreamCell (valueA, restA), StreamCell (valueB, restB)) ->
          if (Element.compare valueA valueB) <= 0
            then Stream.insert valueA (mergeStream restA streamB)
            else Stream.insert valueB (mergeStream streamA restB)
    ;;

    (* Execute one item of the schedule *)
    let rec execSchedule (schedule: schedule): schedule = match schedule with
      | [] -> []
      (* Didn't evaluate anything on the schedule. Look for one in the next
      stream *)
      | (lazy Nil) :: restSchedule -> execSchedule restSchedule
      (* Force just the first item of the stream *)
      | (lazy (StreamCell (firstElem, restStream))) :: restSchedule ->
        restStream :: restSchedule
    ;;

    (* Execute an item of the schedule associated with a schedule, twice *)
    let rec execScheduleFromSegment ({data; schedule}: segment): segment =
      {data; schedule = execSchedule (execSchedule schedule)}
    ;;

    let empty: sortable = {size = 0; segments = []};;

    let rec addSegment
      (stream: t stream)
      (segments: segment list)
      (size: int)
      (* Schedule is constructed in reverse order *)
      (revSchedule: schedule): segment list =
      if size mod 2 == 0
        (* This means no segment was found on this 'spot'. Just add the stream
           here *)
        then {data = stream; schedule = List.rev revSchedule} :: segments
        (* Otherwise we need to merge with the current stream *)
        else
          let ({data = currentStream; schedule}) :: restSegments =
            segments in
          (* The schedule associated to that stream must already by executed at
             this point. *)
          let () = assert (schedule == []) in
          let newStream = mergeStream stream currentStream in
          (* Try to add the merged stream in the next position *)
          addSegment
            newStream
            restSegments
            (size / 2)
            (newStream :: revSchedule)
    ;;

    (* @see IBottomUpMergeSort *)
    let addElement (element: t) ({size; segments}: sortable): sortable =
      let newSegments =
        addSegment (Stream.singleton element) segments size [] in
      {size = size + 1; segments = List.map execScheduleFromSegment newSegments}
    ;;

    (* Merge segments of different sizes. This is only performed when requesting
       a 'sort' *)
    let rec mergeAll (stream: t stream) (segments: segment list): t stream =
      match segments with
        | [] -> stream
        | ({data = currentStream}) :: restSegments ->
          mergeAll (mergeStream stream currentStream) restSegments
    ;;

    (* @see IBottomUpMergeSort *)
    let sort ({segments}: sortable): t list =
      Stream.toList (mergeAll Stream.empty segments)
    ;;
end;;
