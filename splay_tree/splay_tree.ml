(*
  Binary search tree. The value of the root node is greater than the values from
  its left subtree and less or equal than the values from its right subtree.
*)
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

exception Empty_tree_exception;;

(*
  Returns the subtree of tree containing all the elements that are smaller or
  equal than pivot. It doesn't perform the rebalancing. See @smaller
*)
let rec partition pivot tree = match tree with
  | Empty -> (Empty, Empty)
  | Node (left, elem, right) as t ->
    if elem <= pivot then match right with
    (*
      Invariants:
        small = left, elem, small(right)
        big = big(right)
    *)
      | Empty -> (t, Empty)
      | Node (rightLeft, rightElem, rightRight) ->
        if rightElem <= pivot then
          (*
            Invariants:
              small: left, elem, (rightLeft, rightElem, small(rightRight))
              + pivoting: (left, elem, rightLeft), rightElem, small(rightRight)

              big: big(rightRight)
          *)
          let (small, big) = partition pivot rightRight
          in (
            Node (
              Node (left, elem, rightLeft),
              rightElem,
              small
            ),
            big
          )
        else (* rightElem > pivot *)
          (*
            Invariants:
              small: left, elem, small(rightLeft)
              big: big(rightLeft), rightElem, rightRight
          *)
          let (small, big) = partition pivot rightLeft
          in (
            Node (
              left,
              elem,
              small
            ),
            Node (
              big,
              rightElem,
              rightRight
            )
          )
    else (* elem > pivot *) match left with
      (*
        Invariants:
          small = small(left)
          big = big(left), elem, right
      *)
      | Empty -> (Empty, t)
      | Node (leftLeft, leftElem, leftRight) ->
        if leftElem > pivot then
          (*
            small: small(leftLeft)

            big: (big(leftLeft), leftElem, leftRight), elem, right
            + pivoting: leftLeft, leftElem, (big(leftRight), elem, right)
          *)
          let (small, big) = partition pivot leftLeft
          in (
            small,
            Node (
              big,
              leftElem,
              Node (leftRight, elem, right)
            )
          )
        else (* leftElem <= pivot *)
          (*
            Invariants:
              small: leftLeft, leftElem, small(leftRight)
              big: big (leftRight), elem, right
          *)
          let (small, big) = partition pivot leftRight
          in (
            Node (
              leftLeft,
              leftElem,
              small
            ),
            Node (
              big,
              elem,
              right
            )
          )
;;

let insert elem tree =
  let (small, big) = partition elem tree in
  Node (small, elem, big)
;;

(*
  Find the minimum element in the tree
*)
let rec findMin tree = match tree with
  | Empty -> Empty_tree_exception
    (* because it's a binary search tree, we only need to look for the leftmost
       leaf *)
  | Node (Empty, elem, _) -> elem
  | Node (left, elem, _) -> findMin left
;;

let rec deleteMin tree = match tree with
  | Empty -> Empty
  | Node (Empty, elem, right) -> right
  | Node (Node(Empty, leftElem, leftRight), elem, right) - >
      Node(leftRight, elem, right)
  | Node (Node(leftLeft, leftElem, leftRight), elem, right) ->
      Node (deleteMin leftLeft, leftElem, Node(leftRight, elem, right))
;;

let rec equals tree1 tree2 = match (tree1, tree2) with
  | (Empty, Empty) -> true
  | (Node(left1, elem1, right1), Node(left2, elem2, right2)) ->
    elem1 == elem2 &&
    equals left1 left2 &&
    equals right1 right2
  | _ -> false
;;

let rec toString tree = match tree with
  | Empty -> "N"
  | Node(Empty, elem, Empty) -> (string_of_int elem)
  | Node(left, elem, right) ->
    "(" ^ (toString left) ^ ", " ^ (string_of_int elem)  ^ ", " ^ (toString right) ^ ")"
;;

(* TODO: write tests *)
