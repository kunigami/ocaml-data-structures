(*
  A red-black tree is a binary seach tree in which every node is either Red or
  Black. It respects the following invariants:

  1. No red node has a red child
  2. Every path from the root to an empty node has the same number of black
     nodes

  Property: the height of a red-black tree of n nodes is at most
    2*floor(log(n + 1))

  Proof idea: If the tree has a path to an empty node of length greater than
    2*floor(log(n + 1)), this must have more than floor(log(n + 1)) black nodes
    in the path because we cannot have two consecutive red nodes (invariant 1).
    Now, by removing all the red nodes, we must have a complete tree of
    height >= floor(log(n) + 1) + 1, which means 2**(floor(log(n + 1)) + 1) - 1
    which is greater than n.
*)
type color = Black | Red
type elemType = int
type treeType = Empty | Node of color * treeType * elemType * treeType

(*
  Decide whether an element (needle) exists in the tree. O(height of the tree)
  which we showed is O(log n).
*)
let rec member (tree: treeType) (needle: elemType): bool =
  match tree with
    | Empty -> false
    | Node (_, left, elem, right) ->
      if elem > needle then member left needle
      else if elem < needle then member right needle
      else true
;;

let balance (tree: treeType) =
  match tree with
    | Empty -> Empty
    (*
      ascii representation of each case:
      - a, b, c, d represent subtrees
      - x, y, z represent the elements
      - elements surrounded by [] are black, otherwise red.
      - assumes a, b, c, d are balanced already.
    *)
    | (* Case 1
             [z]
             / \
            y   d
           /\
          x c
         /\
        a b
      *)
      Node (
        Black,
        Node (
          Red,
          Node(Red, a, x, b),
          y,
          c
        ),
        z,
        d
      )
    | (* Case 2
             [z]
             / \
            x  d
           /\
          a y
           /\
          b c
      *)
      Node (
        Black,
        Node (
          Red,
          a,
          x,
          Node(Red, b, y, c)
        ),
        z,
        d
      )
    | (* Case 3
             [x]
             / \
            a  z
              /\
            y d
           /\
          b c
      *)
    Node (
        Black,
        a,
        x,
        Node (
          Red,
          Node (Red, b, y, c),
          z,
          d
        )
      )
    | (* Case 4
             [x]
             / \
            a  y
              /\
             b z
              /\
             c d
      *)
    Node (
        Black,
        a,
        x,
        Node (
          Red,
          b,
          y,
          Node(Red, c, z, d)
        )
      )
      (* Result
                 y
               /  \
             [x] [z]
             /\  /\
            a b c d
        *)
      -> Node (
        Red,
        Node (Black, a, x, b),
        y,
        Node (Black, c, z, d)
      )
    | rest -> rest
;;

(*
  Insert an element in a red-black tree.
*)
let insert (tree: treeType) (elem: elemType) =
  (* The reason we work with an internal function is because we handle the
     root differently *)
  let rec insertInner tree elem = match tree with
    | Empty -> Node (Red, Empty, elem, Empty)
    | Node (color, left, nodeElem, right) ->
      if elem < nodeElem then balance (Node (color, insertInner left elem, nodeElem, right))
      else if elem > nodeElem then balance (Node (color, left, nodeElem, insertInner right elem))
      else tree (* Ignore repeated entries *)
  in
    let result = insertInner tree elem in
    (* Always color the root Black *)
    match result with
      | Empty -> failwith "The tree cannot be empty, since we just inserted an element"
      | (Node (color, left, elem, right)) -> Node (Black, left, elem, right)
;;

let rec equals (tree1: treeType) (tree2: treeType): bool =
  match (tree1, tree2) with
   | (Empty, Empty) -> true
   | (Node (colorA, leftA, elemA, rightA), Node (colorB, leftB, elemB, rightB)) ->
      colorA == colorB && elemA == elemB && (equals leftA leftB) && (equals rightA rightB)
   | (_, _) -> false
;;
