open IRandomAccessList;;

(*
  A binary random access list distributes the elements in complete binary leaf
  trees (cblt). A complete binary leaf tree is one that only stores elements at the
  leaves, so a tree with height i, has 2^(i+1)-1 nodes, but only 2^i elements.

  Consider an array of size N, and let bn be the binary representation of N. If
  the i-th digit of bn is 1, then we have a cblt containing 2^i elements. We
  then distribute the elements into these trees, starting with the least
  significant digit (i.e. the smallest tree) and traversing the tree in
  pre-order.

  Adding a new element to the front consists in creating a singleton (leaf) tree
  and adding it to the list of trees. Given that we can only have one tree of
  any given size, we need to merge trees with the same size. This is analogous
  to incrementing the binary number. The merge operation is called 'link'. There
  are at most O(log n) link operations, and linking is O(1).

  Removing the first element is analogous to decrementing a binary number, which
  can also be done in O(log n).

  Searching/updating an element consists in finding the right tree among the
  digits (at most O(log n) of them) and within a tree it also only needs to
  traverse the height of a complete binary tree, which is at most O(log n), so
  overall it's an O(log n) operation.
*)
module BinaryRandomAccessList: IRandomAccessList =
  struct

    exception IndexOutOfBoundsException;;
    exception EmptyListException;;
    exception ViolationException;;

    type 'a tree = Leaf of 'a  | Node of {
      size: int; (* Number of elements/leaves in the tree - not nodes *)
      left: 'a tree; (* left sub-tree *)
      right: 'a tree; (* right sub-tree *)
    };;

    (* Binary representation of the list *)
    type 'a digit = Zero | One of 'a tree;;

    type 'a t = 'a digit list;;

    let empty = [];;
    let isEmpty ls = List.length ls == 0;;

    let size tree = match tree with
      | Leaf _ -> 1
      | Node ({size}) -> size
    ;;

    let link tree1 tree2 = Node {
      size = (size tree1) + (size tree2);
      left = tree1;
      right = tree2;
    };;

    let rec pushTree tree digits = match digits with
      | [] -> [One tree]
      | Zero :: restDigits -> (One tree) :: restDigits
      | (One currentTree) :: restDigits ->
        Zero :: (pushTree (link tree currentTree) restDigits)
    ;;

    (*
      Returns a pair (t, digits), which consists of extracting the left-most
      leaf of the left-most tree. Recursive calls might return intermediate
      trees.
    *)
    let rec popTree digits = match digits with
      | [] -> raise EmptyListException
      | (One tree) :: [] -> (tree, [])
      | (One tree) :: restDigits -> (tree, Zero :: restDigits)
      | Zero :: restDigits ->
        (* Because of the way the trees are built, the resulting tree cannot be
           a left unless it's the root call to this function *)
        let ((Node {left; right}), newDigits) = popTree restDigits in
          (left, (One right) :: newDigits)
    ;;

    let push element digits = pushTree (Leaf element) digits;;
    let head digits = let (Leaf elem, _) = popTree digits in elem;;
    let tail digits = let (_, newDigits) = popTree digits in newDigits;;

    let rec lookupTree index tree = match tree with
      | Leaf elem ->
        if index == 0 then elem else raise IndexOutOfBoundsException
      | (Node {size; left; right}) -> if index < size / 2
        then lookupTree index left
        else lookupTree (index - size / 2) right
    ;;

    let rec lookup index digits = match digits with
      | [] -> raise IndexOutOfBoundsException
      | Zero :: restDigits -> lookup index restDigits
      | (One tree) :: restDigits ->
        if index < size tree then lookupTree index tree
        else lookup (index - (size tree)) restDigits
    ;;

    let rec updateTree index element tree = match tree with
      | Leaf _ ->
        if index == 0 then (Leaf element) else raise IndexOutOfBoundsException
      | (Node {size; left; right}) -> if index < size / 2
        then Node {size; left = updateTree index element left; right}
        else Node {
          size;
          left;
          right = updateTree (index - size / 2) element right;
        }
    ;;

    let rec update index element digits = match digits with
      | [] -> raise IndexOutOfBoundsException
      | Zero :: restDigits -> Zero :: (update index element restDigits)
      | (One tree) :: restDigits ->
        if index < size tree
          then (One (updateTree index element tree)) :: restDigits
          else (One tree) :: (update (index - (size tree)) element restDigits)
    ;;
  end
;;
