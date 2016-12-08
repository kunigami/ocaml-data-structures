open OUnit2;;
open Splay_tree;;

(*
  tree:  8
        / \
      4   12
     /\  / \
    2 6 10 14
*)
let leaf n = Node (Empty, n, Empty);;

let exampleTree = Node(
  Node(leaf 2, 4, leaf 6),
  8,
  Node(leaf 10, 12, leaf 14)
)
;;

let testPartitionEmptyTree text_ctx =
  let (small, big) = Splay_tree.partition 10 Empty in
  let () = assert_bool "Small check" (Splay_tree.equals small Empty) in
  assert_bool "Big check" (Splay_tree.equals big Empty)
;;

let testPartitionSingletonTreeCase1 text_ctx =
  let (small, big) = Splay_tree.partition 10 (leaf 10) in
  let () = assert_bool "Small check" (Splay_tree.equals small (leaf 10)) in
  assert_bool "Big check" (Splay_tree.equals big Empty)
;;

let testPartitionSingletonTreeCase2 text_ctx =
  let (small, big) = Splay_tree.partition 9 (leaf 10) in
  let () = assert_bool "Small check" (Splay_tree.equals small Empty) in
  assert_bool "Big check" (Splay_tree.equals big (leaf 10))
;;

(*
    Case 1: pivot 13, right + right
    Output:

    small (<=):      rebalanced
      8                   12
     / \                 / \
    4   12              8   N
   /\   / \            /\
  2  6 10  N          4 10
                     /\
                    2 6
   big:
     14
     / \
    N   N
*)
let testPartitionCase1 text_ctx =
  let (small, big) = Splay_tree.partition 13 exampleTree in
  let expectedSmall = (Node (
    Node(Node(leaf 2, 4, leaf 6), 8, leaf 10),
    12,
    Empty
  )) in
  let () = assert_bool "Small check" (
    Splay_tree.equals small expectedSmall
  ) in
  let expectedBig = leaf 14 in
  assert_bool "Big check" (
    Splay_tree.equals big expectedBig
  )
;;

(*
    Case 2: pivot 11, right + left
    Output:

    small (<=):
      8
     / \
    4   10
   /\
  2  6

   big:
     12
     / \
    N   14
*)
let testPartitionCase2 text_ctx =
  let (small, big) = Splay_tree.partition 11 exampleTree in
  let expectedSmall = Node(Node(leaf 2, 4, leaf 6), 8, leaf 10) in
  let () = assert_bool "Small check" (
    Splay_tree.equals small expectedSmall
  ) in
  let expectedBig = Node(Empty, 12, leaf 14) in
  assert_bool "Big check" (
    Splay_tree.equals big expectedBig
  )
;;

(*
    Case 3: pivot 3, left + left
    Output:

    small (<=):
      2
     / \
    N  N

   big:             rebalanced
          8             4
         / \           / \
       4   12         N  8
      /\  / \           / \
     N 6 10 14         6  12
                          / \
                        10  14
*)
let testPartitionCase3 text_ctx =
  let (small, big) = Splay_tree.partition 3 exampleTree in
  let expectedSmall = (leaf 2) in
  let () = assert_bool "Small check" (
    Splay_tree.equals small expectedSmall
  ) in
  let expectedBig = Node(
    Empty,
    4,
    Node(leaf 6, 8, Node(leaf 10, 12, leaf 14))
  ) in
  let () = print_string (Splay_tree.toString big) in
  assert_bool "Big check" (
    Splay_tree.equals big expectedBig
  )
;;

(*
    Case 4: pivot 5, left + right
    Output:

    small (<=):
       4
      /\
     2  N

   big:
      8
     / \
    6  12
       / \
      10 14
*)
let testPartitionCase4 text_ctx =
  let (small, big) = Splay_tree.partition 5 exampleTree in
  let expectedSmall = Node(leaf 2, 4, Empty) in
  let () = assert_bool "Small check" (
    Splay_tree.equals small expectedSmall
  ) in
  let expectedBig = Node(leaf 6, 8, Node(leaf 10, 12, leaf 14)) in
  assert_bool "Big check" (
    Splay_tree.equals big expectedBig
  )
;;

let testInsertingElementEmptyTree text_ctx =
  let newTree = Splay_tree.insert 10 Empty in
  assert_bool "Should be a singleton tree" (
    Splay_tree.equals newTree (leaf 10)
  )
;;

(*
    Case 1: check testPartitionCase1

            13
           /   \
         12    14
        / \
       8   N
      /\
     4 10
    /\
   2 6
*)
let testInsertingElementCase1 text_ctx =
  let newTree = Splay_tree.insert 13 exampleTree in
  let expectedTree = Node(
    Node (
      Node(
        Node(
          leaf 2,
          4,
          leaf 6
        ),
        8,
        leaf 10
      ),
      12,
      Empty
    ),
    13,
    leaf 14
  ) in
  assert_bool "Should cause the tree to be rotated" (
    Splay_tree.equals newTree expectedTree
  )
;;

let suite =
"suite">:::
 [
  "testPartitionEmptyTree">:: testPartitionEmptyTree;
  "testPartitionSingletonTreeCase1">:: testPartitionSingletonTreeCase1;
  "testPartitionSingletonTreeCase2">:: testPartitionSingletonTreeCase2;
  "testPartitionCase1">:: testPartitionCase1;
  "testPartitionCase2">:: testPartitionCase2;
  "testPartitionCase3">:: testPartitionCase3;
  "testPartitionCase4">:: testPartitionCase4;
  "testInsertingElementEmptyTree">:: testInsertingElementEmptyTree;
  "testInsertingElementCase1">:: testInsertingElementCase1;
 ]
;;

let () =
  run_test_tt_main suite
;;
