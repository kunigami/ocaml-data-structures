(*

  A spine of a node is the path defined by following the right children
  until a leaf is found (i.e. the rightmost path). The rank is the length
  of the spine.

  A leftist heap is a binary tree such that the value of a parent is greater
  or equal than its children and the rank of the left child is always greater
  or equal than the right child.

  The rank of a tree with n nodes is O(log n).

*)

open IHeap;;

module LeftistHeap(Element: Set.OrderedType): IHeap with type tv = Element.t =
  struct
    type tv = Element.t;;

    (* We store the rank as part of the node *)
    type heap = Empty | Tree of int * tv * heap * heap

    exception Empty_heap;;

    let empty = Empty
    let isEmpty heap = match heap with
      | Empty -> true
      | _ -> false


    let singleton (elem: tv): heap = Tree(1, elem, Empty, Empty)

    let rank heap =
      match heap with
        | Empty -> 0
        | Tree(r, _, _, _) -> r

    (*
      Construct a heap tree by preserving the leftist property by swapping the
      left and right children if necessary.
    *)
    let makeTree (elem: tv) (left: heap) (right: heap): heap =
      if rank left < rank right then Tree((rank left) + 1, elem, right, left)
      else Tree((rank right) + 1, elem, left, right)

    let rec merge (heapA: heap) (heapB: heap): heap =
      match (heapA, heapB) with
        | (heapA, Empty) -> heapA
        | (Empty, heapB) -> heapB
        | (Tree(r1, elem1, left1, right1), Tree(r2, elem2, left2, right2)) ->
          if elem1 < elem2 then makeTree elem1 left1 (merge right1 heapB)
          else makeTree elem2 left2 (merge right2  heapA)

    let insert (elem: tv) (heap: heap): heap =
      merge heap (singleton elem)

    let findMin (heap: heap): tv =
      match heap with
        | Empty -> raise Empty_heap
        | Tree(_, elem, _, _) -> elem

    let deleteMin (heap: heap) =
      match heap with
        | Empty -> Empty
        | Tree (_, _, left, right) -> merge left right

    let rec equals (heapA: heap) (heapB: heap) =
      match (heapA, heapB) with
        | (Empty, Empty) -> true
        | (heapA, Empty) -> false
        | (Empty, heapB) -> false
        | (Tree(r1, elem1, left1, right1), Tree(r2, elem2, left2, right2)) ->
          elem1 == elem2 &&
          r1 == r2 &&
          (equals left1 left2) &&
          (equals right1 right2)
end
