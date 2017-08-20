open IRandomAccessList;;

(*

  A skew binary random access list is similar to the BinaryRandomAccessList
  except it uses a modified numeric representation called skewed binary. Here
  are the rules:

  The weight of the i-th digit is 2^(i+1) - 1. It allows the least siginificant
  non-zero digit to be 2. It's possible to show this number system offers a
  unique representation. Examples:

  1 -> 1
  2 -> 2
  3 -> 10
  4 -> 11
  5 -> 12
  6 -> 20
  7 -> 100
  8 -> 101
  9 -> 102
  10 -> 110
  11 -> 111
  12 -> 112
  13 -> 120
  14 -> 200
  15 -> 1000

  Incrementing a number follows these rules:
    * If there's a digit 2 in the number, turn it into 0 and increment the next
      digit. By definition that is either 0 or 1, so we can safely increment it
      without having to continue carrying over.
    * Otherwise the least sigifinicant digit is either 0 or 1, and it can be
      incremented without carry overs.

  The advantage of this number system is that increments (similarly, decrements)
  never carry over more than once so the complexity O(1), as opposed to
  possibly O(log n) for regular binary numbers.

  A skew binary random access list can be implemented using this idea. We use a
  sparse representation (do not include the 0s). Each digit one with position i
  corresponds to a tree with (2^(i+1) - 1) elements, in this case a complete
  binary tree with height i+1. A digit 2 is represented by two consecutive trees
  with same weight. Adding a new element to the beginning of the list is
  analogous to incrementing the number, which we saw can be done in O(1).

  Elements are inserted in pre-order in each tree, so when searching for an
  index, we can first find the right tree by looking at the tree sizes and
  within a tree we can do a "binary search" in O(height) of the tree.
*)
module SkewBinaryRandomAccessList: IRandomAccessList =
  struct

    exception IndexOutOfBoundsException;;
    exception EmptyListException;;

    type 'a node = Leaf of 'a  | Node of {
      element: 'a;
      left: 'a node; (* left sub-tree *)
      right: 'a node; (* right sub-tree *)
    };;

    type 'a tree = {size: int; tree: 'a node};;

    type 'a t = 'a tree list;;

    let empty = [];;
    let isEmpty ls = List.length ls == 0;;

    (*
      Equivalent to incrementing a skewed binary digit.

      Converting a digit 0 to 1 or 1 to 2, is a matter of prepending a tree to a
      list. To convert a 2 to 0 and increment the next position, we need to
      merge two trees representing it with the element to be inserted. Because
      each tree is traversed in pre-order, we make the element the root of the
      tree.
    *)
    let push element ls = match ls with
      | {size = size1; tree = tree1} ::
        {size = size2; tree = tree2} ::
        rest -> if size1 == size2 (* First non-zero digit is 2 *)
          then {
            size = 1 + size1 + size2;
            tree = Node {element; left = tree1; right = tree2}
          } :: rest
          (* Add a tree of size 1 to the beginning, analogous to converting a
          digit 0 to 1, or 1 to 2 *)
          else {size = 1; tree = Leaf element} :: ls
      | _ -> {size = 1; tree = Leaf element} :: ls
    ;;

    let head ls = match ls with
      | [] -> raise EmptyListException
      | {tree = Leaf element} :: _ -> element
      | {tree = (Node {element})} :: _ -> element
    ;;

    let tail ls = match ls with
      | [] -> raise EmptyListException
      | {tree = Leaf element} :: rest -> rest
      | {size; tree = Node {element ; left; right}} :: rest ->
        {size = size / 2; tree = left} ::
        {size = size / 2; tree = right} ::
        rest
    ;;

    let rec lookupTree index tree size = match tree with
      | Leaf element -> element
      | Node {element; left; right} ->
        let newSize = ((size + 1) / 2) - 1 in
        if index == 0 then element
        else if index <= newSize then lookupTree (index - 1) left newSize
        else lookupTree (index - newSize - 1) right newSize
    ;;

    let rec lookup index ls = match ls with
       | [] -> raise IndexOutOfBoundsException
       | {size; tree} :: rest -> if index < size
         then lookupTree index tree size
         else lookup (index - size) rest
    ;;

    let rec updateTree index newElement tree size = match tree with
      | Leaf element -> Leaf newElement
      | Node {element; left; right} ->
        let newSize = ((size + 1) / 2) - 1 in
        if index == 0 then Node {element = newElement; left; right}
        else if index <= newSize then
          Node {
            element;
            left = updateTree (index - 1) newElement left newSize;
            right;
          }
        else
          Node {
            element;
            left;
            right = updateTree (index - newSize - 1) newElement right newSize;
          }
    ;;

    let rec update index element ls = match ls with
      | [] -> raise IndexOutOfBoundsException
      | {size; tree} :: rest -> if index < size
        then {size; tree = updateTree index element tree size} :: rest
        else {size; tree} :: (update (index - size) element rest)
    ;;
  end
;;
