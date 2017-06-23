(*

  A binomial tree can be defined recursively based on its rank. The base is a
  tree of rank 0, which is a single node. A tree of rank r > 0, is formed by
  combining two trees of rank r-1 making one tree the leftmost child of the
  other. Examples:

  rank 0: o

  rank 1: o
          |
          o

  rank 2: o
         /|
        o o
        |
        o

  rank 3: o
       / /|
      o o o
     /| |
    o o o
    |
    o

  A binomial heap is a list of binomial trees none of which has repeated ranks
  and for each binomal tree the value of a node is always greater or equal to
  its children.
*)

open Set;;
open Stream2;;

module Stream = Stream2;;

module Scheduled_binomial_heap(Element: Set.OrderedType) =
  struct
    type tree = Node of Element.t * tree list;;
    type tv = Element.t
    (*
      A heap with n elements can be associated to the binary representation of n.
      The 0's correspond to no tree, while the 1s in position i correspond to trees with size 2^i
    *)
    type digit = Zero | One of tree;;
    type schedule = digit stream list;;
    type heap = {
      digits: digit stream;
      schedule: schedule;
    };;

    exception Empty_heap;;

    let empty = {
      digits = Stream.empty;
      schedule = [];
    }
    ;;

    let isEmpty (heap: heap): bool = match heap with
      {digits; schedule} ->
        let forced_digit = Lazy.force digits in
        match forced_digit with
          | Nil -> true
          | _ -> false
    ;;

    (*
      Links two trees of same rank r. The resulting tree has rank r + 1
    *)
    let link (tree1: tree) (tree2: tree) = match (tree1, tree2) with
      (Node (node1, children1), Node (node2, children2)) ->
        if (Element.compare node1 node2) <= 0
          then Node (node1, tree2 :: children1)
          else Node (node2, tree1 :: children2)
    ;;

    (*
      Insert a tree into a stream of digits. This has a strong assumption that the rank of the tree
      being inserted has the right rank associated with the position of the current digits.

      This is analogous to the carrying over operation of adding a 1 to a binary number. For
      example, if we are to add 1 to 1011, then we'll have

      101(11) -> match One, link -> 10(11)0
      10(11)0 -> match One, link -> 1(1)00
      1(1)00 -> match Zero -> 1100
    *)
    let rec insertTree (tree: tree) (digits: digit stream): digit stream =
      let forcedDigits = Lazy.force digits in match forcedDigits with
        | Nil -> Stream.empty |> Stream.insert (One tree)
        | StreamCell (firstDigit, rest) -> match firstDigit with
          | Zero -> Stream.insert (One tree) rest
          | One firstTree -> Stream.insert Zero (insertTree (link tree firstTree) rest)
    ;;

    (*
      Execute or evaluate the first item of the schedule. If the first digit of the item is Zero, we
      re-schedule the rest of the digits. If it's a One, it means that the remaining digits have
      been executed, so we just discard it.
    *)
    let execSchedule (schedule: digit stream list): digit stream list = match schedule with
      | [] -> []
      | firstItem :: rest ->
        let forcedFirstItem = Lazy.force firstItem in match forcedFirstItem with
          | StreamCell (Zero, job) -> job :: rest
          | _ -> rest
    ;;

    (*
      Inserts an element in the heap.
    *)
    let insert (elem: tv) (heap: heap): heap =
      match heap with {digits; schedule} ->
        let newDigits = insertTree (Node (elem, [])) digits in
        let newSchedule = execSchedule (newDigits :: schedule) in
        {digits = newDigits; schedule = newSchedule}
    ;;

    (* Helper function to extract the root from a tree *)
    let getRoot (tree: tree): Element.t = match tree with
      Node (root, _) -> root
    ;;

    (*
      Given the digits representing the non-scheduled part of the heap, we scan through it to find
      the tree (digit) with the smallest root.
    *)
    let rec removeMinTree (digits: digit stream): (tree * digit stream) =
      let forcedDigits = Lazy.force digits in match forcedDigits with
        | Nil -> raise Empty_heap

        (* No tree here. Keep searching *)
        | StreamCell (Zero, rest) ->
          let (newTree, newDigits) = removeMinTree rest in
          (newTree, Stream.insert Zero newDigits)

        (* Tree found *)
        | StreamCell (One tree, rest) -> (
          let forcedRestDigits = Lazy.force rest in match forcedRestDigits with
            (* This was the last digit. Return the tree *)
            | Nil -> (tree, Stream.empty)
            (* Compare the root of the current tree with the minimum found so far. *)
            | _ ->
              let root = getRoot tree in
              let (minTree, newDigits) = removeMinTree rest in
              let minRoot = getRoot minTree in
                if ((Element.compare root minRoot) <= 0) then (tree, Stream.insert Zero rest)
                else (minTree, Stream.insert (One tree) newDigits)
          )
    ;;

    let findMin (heap: heap): tv = match heap with {digits} ->
      let (minTree, _) = removeMinTree digits in
      getRoot minTree
    ;;

    let rec evaluateDigits (digits: digit stream): digit stream =
      let forcedDigits = Lazy.force digits in match forcedDigits with
        | Nil -> digits
        | StreamCell (_, rest) ->
          (* We don't need the results. Just evaluate the stream *)
          let () = ignore (evaluateDigits rest) in
          digits
    ;;

    let rec mergeDigits (digitsA: digit stream) (digitsB: digit stream) =
      let forcedDigitsA = Lazy.force digitsA in
      let forcedDigitsB = Lazy.force digitsB in
      match (forcedDigitsA, forcedDigitsB) with
        | (_, Nil) -> digitsA
        | (Nil, _) -> digitsB
        | (StreamCell(Zero, restDigitsA), StreamCell(digitB, restDigitsB)) ->
          Stream.insert digitB (mergeDigits restDigitsA restDigitsB)
        | (StreamCell(digitA, restDigitsA), StreamCell(Zero, restDigitsB)) ->
          Stream.insert digitA (mergeDigits restDigitsA restDigitsB)
        | (
            StreamCell(One treeA, restDigitsA),
            StreamCell(One treeB, restDigitsB)
          ) ->
          let treeToInsert = link treeA treeB in
          let mergedDigits = mergeDigits restDigitsA restDigitsB in
          Stream.insert Zero (insertTree treeToInsert mergedDigits)
    ;;

    let deleteMin (heap: heap): heap = match heap with {digits} ->
      let (Node (elem, children), newDigits) = removeMinTree digits in
      let digitsFromChildren =
        Stream.fromList (List.map (fun tree -> One tree) (List.rev children)) in
      let newMergedDigits = mergeDigits digitsFromChildren newDigits in
      {digits = evaluateDigits newMergedDigits; schedule = []}
    ;;
end;;
