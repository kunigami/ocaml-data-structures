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

open IHeap;;

module Binomial_heap(Element: Set.OrderedType): IHeap with type tv = Element.t =
  struct
    type rankType = int;;
    type tv = Element.t;;
    (*
      The list of nodes in the subtree list is of decreasing order of rank. For a
      rank 3 tree, the list contains a list of [rank=2, rank=1, rank=0].
    *)
    type tree = Node of rankType * tv * tree list;;
    (*
      The list of nodes in tree list is of increasing order of rank (note it's the
      opposite order of the tree).
    *)
    type heap = tree list;;

    exception Invariant_violation of string;;
    exception Empty_heap;;

    let empty = [];;

    let isEmpty (heap: heap): bool = match heap with
      | [] -> true
      | _ -> false
    ;;

    (*
      Construct a node by combining two nodes of the same rank. This is the
      induction step on the definition of the tree.
    *)
    let link (treeA: tree) (treeB: tree): tree =
      match (treeA, treeB) with
        | (Node (rankA, elemA, childrenA), Node (rankB, elemB, childrenB)) ->
          let () = assert (rankA == rankB) in
          if (Element.compare elemA elemB) <= 0
            then Node (rankA + 1, elemA, treeB :: childrenA)
            else Node (rankA + 1, elemB, treeA :: childrenB)
    ;;

    (*
      Helper methods
    *)
    let rank (Node (rank, _, _)) = rank;;
    let root (Node (_, elem, _)) = elem;;
    let singletonTree (elem: tv) = Node (1, elem, []);;

    (*
      Inserts a tree inside a heap.
      Invariant: trees in the heap are sorted by increasing order of rank. No
        repeated ranks.
    *)
    let rec insertTree (tree: tree) (heap: heap): heap =
      match (tree, heap) with
        | (tree, []) -> [tree]
        | (tree, (lowestRankTree :: rest as heapTrees)) ->
          if rank tree < rank lowestRankTree then
            tree :: heapTrees
          else if rank tree == rank lowestRankTree then
            insertTree (link tree lowestRankTree) rest
          else
            failwith "This condition should not happen. The heap would be empty"
    ;;

    (*
      @see IHeap
    *)
    let insert (element: tv) (heap: heap): heap =
      insertTree (singletonTree element) heap
    ;;

    (*
      Merge two heaps by merging their inner trees. Maintains the binomial heap
      invariants.
    *)
    let rec merge (heapA: heap) (heapB: heap) =
      match (heapA, heapB) with
        | (heapA, []) -> heapA
        | ([], heapB) -> heapB
        | ((headTreeA :: restA as heapA), (headTreeB :: restB as heapB)) ->
          if rank headTreeA > rank headTreeB then
            headTreeA :: (merge restA heapB)
          else if rank headTreeA < rank headTreeB then
            headTreeB :: (merge heapA restB)
          else (link headTreeA headTreeB) :: (merge restA restB)
    ;;

    (*
      Remove the tree with the minimum root from the list, returning both the tree
      and the remainder of the list.
    *)
    let rec removeMinTree (heap: heap) =
      match heap with
        | [] -> raise Empty_heap
        | [tree] -> (tree, [])
        | tree :: rest ->
            let (minTree, minRest) = removeMinTree rest in
              if (Element.compare (root tree) (root minTree)) <= 0
                then (tree, rest)
                else (minTree, tree :: minRest)
    ;;

    (*
      Returns the minimum element from the heap.
    *)
    let findMin (heap: heap): tv =
      let (minTree, _) = removeMinTree heap in
      root minTree
    ;;

    (*
      Removes the minimum element from the heap.
    *)
    let deleteMin (heap: heap): heap =
      let (minTree, rest) = removeMinTree heap in
      match minTree with (Node (_, _, children)) ->
        merge (List.rev children) rest
    ;;
end;;
