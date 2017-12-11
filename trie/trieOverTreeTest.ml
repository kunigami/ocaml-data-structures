open Map;;
open TrieOverTree;;
open OUnit2;;

module IntMap = Map.Make(Int64)
module TrieOverIntTree = TrieOverTree(IntMap)

include TrieOverIntTree

let testQueryOnEmptyTrie text_ctx =
  let emptyTrie = empty in
  assert_bool
    "Empty trie should always return false"
    (not (has Empty emptyTrie))
;;

let testQueryOnSingleNodePath text_ctx =
  let tree = Node (11L, Empty, Empty) in
  let emptyTrie = empty in
  let trie = insert tree 10L emptyTrie in
  let elem = find tree trie in
  assert_bool
    "Should find the element using a tree as 'address'"
    (elem = 10L)
;;

let testWithDifferentInstancesOfTree text_ctx =
  let treeInsert = Node (11L, Empty, Empty) in
  let treeQuery = Node (11L, Empty, Empty) in
  let emptyTrie = empty in
  let trie = insert treeInsert 10L emptyTrie in
  let elem = find treeQuery trie in
  assert_bool
    "Should find the element using diffent trees for insertion and find"
    (elem = 10L)
;;

let testWithDifferentTrees text_ctx =
  let treeInsert = Node (11L, Empty, Empty) in
  (* These trees differ on the root element *)
  let treeQuery = Node (12L, Empty, Empty) in
  let emptyTrie = empty in
  let trie = insert treeInsert 10L emptyTrie in
  assert_raises
    ~msg:"Should not find if the trees differ"
    Not_found
    (fun () -> find treeQuery trie)
;;

let testWithCompleteTree text_ctx =
  let tree = Node (10L, Node (11L, Empty, Empty), Node (12L, Empty, Empty)) in
  let emptyTrie = empty in
  let trie = insert tree 10L emptyTrie in
  let elem = find tree trie in
  assert_bool
    "Should find the element using diffent trees for insertion and find"
    (elem = 10L)
;;

let testWithDifferentCompleteTree text_ctx =
  let treeInsert = Node (10L, Node (11L, Empty, Empty), Node (12L, Empty, Empty)) in
  let treeQuery = Node (
    10L,
    Node (11L, Empty, Empty),
    Node (12L, Node (13L, Empty, Empty), Empty)
  ) in
  let emptyTrie = empty in
  let trie = insert treeInsert 10L emptyTrie in
  assert_raises
    ~msg:"Should not find if the trees differ"
    Not_found
    (fun () -> find treeQuery trie)
;;


let testInsertMultipleTrees text_ctx =
  let tree1 = Node (10L, Node (11L, Empty, Empty), Node (12L, Empty, Empty)) in
  let tree2 = Node (10L, Node (11L, Empty, Empty), Node (13L, Empty, Empty)) in
  let tree3 = Node (10L, Node (11L, Empty, Empty), Empty) in
  let tree4 = Node (11L, Node (11L, Empty, Empty), Empty) in
  let trie =
    empty |>
    insert tree1 10L |>
    insert tree2 11L |>
    insert tree3 12L |>
    insert tree4 13L
  in
  let trees = [tree1; tree2; tree3; tree4] in
  let elems = List.map (fun tree -> find tree trie) trees  in
  assert_bool
    "Should find all the elements inserted"
    (elems = [10L; 11L; 12L; 13L])
;;

let suite =
"suite">:::
 [
  "testQueryOnEmptyTrie">:: testQueryOnEmptyTrie;
  "testQueryOnSingleNodePath">:: testQueryOnSingleNodePath;
  "testWithDifferentInstancesOfTree">:: testWithDifferentInstancesOfTree;
  "testWithDifferentTrees">:: testWithDifferentTrees;
  "testWithCompleteTree">:: testWithCompleteTree;
  "testWithDifferentCompleteTree">:: testWithDifferentCompleteTree;
  "testInsertMultipleTrees">:: testInsertMultipleTrees;
 ]
;;

let () =
  run_test_tt_main suite
;;
