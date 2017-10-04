open BatPervasives;;
open IHeap;;

(*
  Binomial heap based on skewed binary numbers. Skewed binary number
  representation allows incrementing and decrementing in O(1).

  Refer to random_access_list/skwedBinaryRandomAccessList.ml for a description
  of skew binary numbers.

  Refer to binomial_heap.ml for a description of binomial trees and ranks.
  A skewed binomial tree of rank r has a list of extra elements up to r.

  The position of the digits in the skewed binary number correspond to the rank
  of the tree associated to that digit. And a digit 2 is represented by two tree
  of the same rank. For example 10120 represents a heap with a tree with rank 4,
  one with rank 2 and two with rank 1.

  There's no correspondence between the size of the heap and the rank of the
  binomial trees. For example a heap of size 4 doesn't necessarily correspond
  its skewed binary representation, "11" (i.e one with rank 0, one with rank 1),
  in terms of ranks. That's because the extra elements change the size of the
  tree.

  = Insertion

  Since in the canonical skewed number representation a 2 cannot be followed
  by a 1, we cannot simply add a singleton tree of rank 0 when inserting into a
  heap with a 2. Instead we merge those two trees and put an element aside (this
  operation is called skewedLink). For example, in 10120 we'll link the 2 trees
  of rank 1 to obtain a new heap corresponding to 10200. Insertion is analogous
  to incrementing a skewed binary number and hence O(1)

  = Find the minimum element

  Because both link and skew link makes sure the smallest element is always the
  root of the tree, findMin only needs to look at the roots of the trees, which
  are at most O(log n) elements.

  = Remove the minimum element

  Because we're removing the smallest element, there's a chance that the next
  smallest element is in the extra elements list of the removed tree, so these
  need to be re-inserted. Note that this smallest element cannot be in any other
  extra elements of any other node, because by construction they were checked
  against the main element of the node before being put in the list.

  We can see that the number of elements in the extra list is at most the rank
  of the current tree, because we only ever add an element to this list
  following a link, which also increases the rank of the tree, so at most O(log
  n) insertions are needed.

  By the definition of the binomial tree we also have O(log n) children in a
  node, so reversing this list is O(log n). Merging two lists is proportional to
  the sum of their lengths, hence O(log n).

  We can then conclude that removing the minimum tree operation is O(log n).
*)
module SkewBinomialHeap(Element: Set.OrderedType):
  IHeap with type tv = Element.t =

  struct

    type tv = Element.t;;

    type node = {
      rank: int;
      element: tv;
      extraElements: tv list;
      children: node list;
    };;

    exception Empty_heap;;

    type heap = node list;;

    let empty = [];;

    let isEmpty heap = match heap with
      | [] -> true
      | _ -> false
    ;;

    (* Extract the rank from a node *)
    let rank {rank} = rank;;

    (* Extract the value from a node *)
    let root {element} = element;;

    let link tree1 tree2 = match (tree1, tree2) with
      ({
        rank = rank1;
        element = element1;
        extraElements = extraElements1;
        children = children1;
      },
      {
        rank = rank2;
        element = element2;
        extraElements = extraElements2;
        children = children2;
      }) ->
      (* Invariant rank1 = rank2 *)
      if (Element.compare element1 element2) <= 0
        then {
          rank = rank1 + 1;
          element = element1;
          extraElements = extraElements1;
          children = tree2 :: children1;
        }
        else {
          rank = rank2 + 1;
          element = element2;
          extraElements = extraElements2;
          children = tree1 :: children2;
        }
    ;;

    let skewLink newElement tree1 tree2 =
      let {rank; element; extraElements; children} = link tree1 tree2 in
      if Element.compare newElement element <= 0 then {
        rank;
        element = newElement;
        extraElements = element :: extraElements;
        children;
      }
      else {
        rank;
        element;
        extraElements = newElement :: extraElements;
        children;
      }
    ;;

    let rec insertTree tree heap = match heap with
      | [] -> [tree]
      | firstTree :: restHeap -> if rank tree < rank firstTree
        then tree :: heap
        else firstTree :: (insertTree tree restHeap)
    ;;

    let insert element heap = match heap with
      | tree1 :: tree2 :: restHeap -> if rank tree1 == rank tree2
        then (skewLink element tree1 tree2) :: restHeap
        else {rank = 0; element; children = []; extraElements = []} :: heap
      | _ -> {rank = 0; element; children = []; extraElements = []} :: heap
    ;;

    let rec mergeTreeListsNormalized heap1 heap2 = match (heap1, heap2) with
      | ([], heap2) -> heap2
      | (heap1, []) -> heap1
      | (firstTree1 :: restHeap1, firstTree2 :: restHeap2) ->
        if rank firstTree1 < rank firstTree2 then
          firstTree1 :: firstTree2 ::
            (mergeTreeListsNormalized restHeap1 restHeap2)
        else if rank firstTree1 > rank firstTree2 then
          firstTree2 :: firstTree1 ::
            (mergeTreeListsNormalized restHeap1 restHeap2)
        else
          (link firstTree1 firstTree2) ::
            (mergeTreeListsNormalized restHeap1 restHeap2)
    ;;

    let linkLeadingTreesWithSameRank heap = match heap with
      | [] -> []
      (* By explicitly inserting the tree in the heap, we'll also handle trees
         with duplicated rank through linking *)
      | firstTree :: restHeap -> insertTree firstTree restHeap

    let mergeTreeLists heap1 heap2 =
      mergeTreeListsNormalized
        (linkLeadingTreesWithSameRank heap1)
        (linkLeadingTreesWithSameRank heap2)
    ;;

    let rec removeMinTree heap = match heap with
      | [] -> raise Empty_heap
      | [tree] -> (tree, [])
      | tree :: restHeap ->
        let (minTree, restRestHeap) = removeMinTree restHeap in
        if Element.compare (root tree) (root minTree) < 0 then (tree, restHeap)
        else (minTree, tree :: restRestHeap)
    ;;

    let findMin heap =
      let (minTree, _) = (removeMinTree heap) in root minTree
    ;;

    let deleteMin heap =
      let ({children; extraElements}, restHeap) = (removeMinTree heap) in
      let newHeap = mergeTreeLists (List.rev children) restHeap in
      (* Insert the remaining elements *)
      List.fold_left (BatPervasives.flip insert) newHeap extraElements
    ;;

  end
;;
