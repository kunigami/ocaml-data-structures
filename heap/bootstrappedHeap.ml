(*

  A bootstrapped heap is a heap designed using a technique called structural
  abstraction (according to Okasaki). It extends an existing implementation of a
  heap to obtain a heap with more efficient merge() operation.

  In particular, it can make enhance a heap with O(log n) merge into one with
  one with O(1).

  To distinguish between the heaps, we call the original heap the 'primary heap'
  and the enhanced one 'bootstrapped heap'. A boostrapped heap is composed of
  an element and the rest of elements are stored in the primary heap. The catch
  is that each element in the primary heap is a boostrapped heap, being this a
  case of mutally recursive structures.

  The merge operation is very simple because merging two boostrapped heaps is
  equivalent to inserting an element in the primary heap. The insert can be
  trivially defined in terms of merge.

  The findMin is also trivial because we always store the element at the root.

  The more complex operation is removeMin, where we need to merge two merge two
  primary heaps (that's why the primary heap has to be IHeapWithMerge - IHeap
  not being enough)
*)

open IHeapWithMerge;;
open IHeap;;
open SkewBinomialHeap;;

module BootstrappedHeap
  (Element: Set.OrderedType)
  (MakeHeap: functor (Element: Set.OrderedType) -> IHeapWithMerge with type tv = Element.t)
: IHeap with type tv = Element.t =
  struct

    module rec BootstrappedElement: sig
      type t = Empty | Heap of Element.t * PrimaryHeap.heap
      (* Include the methods from Set.OrderedType since this will be fed to a
         heap expecting such methods *)
      include Set.OrderedType with type t := t
    end = struct
      type t = Empty | Heap of Element.t * PrimaryHeap.heap
      let compare heap1 heap2 = match (heap1, heap2) with
        | (Heap (x, _), Heap (y, _)) -> Element.compare x y
    end
    and PrimaryHeap: IHeapWithMerge
      with type tv := BootstrappedElement.t = MakeHeap(BootstrappedElement);;

    (* Make the constructs defined in this element available without
      qualification *)
    include BootstrappedElement

    type tv = Element.t;;
    type heap = BootstrappedElement.t;;

    exception Empty_heap;;

    let empty = Empty

    let isEmpty heap = match heap with
      | Empty -> true
      | _ -> false

    (* To merge two heaps, we keep the heap with the smallest element and insert
       the largest heap as an element into the primary heap. This operation is
       O(1) *)
    let merge (heap1: heap) (heap2: heap) = match (heap1, heap2) with
      | (Empty, heap2) -> heap2
      | (heap1, Empty) -> heap1
      | (Heap (element1, primaryHeap1), Heap (element2, primaryHeap2)) ->
        if ((Element.compare element1 element2) < 0)
          then Heap(element1, PrimaryHeap.insert heap2 primaryHeap1)
          else Heap(element2, PrimaryHeap.insert heap1 primaryHeap2)

    let singleton (elem: tv): heap = Heap(elem, PrimaryHeap.empty)

    let insert (elem: tv) (heap: heap): heap =
      merge heap (singleton elem)

    (* Find the smallest element is trivial, since we always have it at the
       root *)
    let findMin (heap: heap): tv =
      match heap with
        | Empty -> raise Empty_heap
        | Heap(elem, _) -> elem

    let deleteMin (heap: heap) =
      match heap with
        | Empty -> raise Empty_heap
        | Heap(_, primaryHeap) ->
          if PrimaryHeap.isEmpty primaryHeap then Empty
          else
            (* Extract the boostrapped heap with the smallest element, remove it
               from the primary heap. Make the smallest element the new root and
               merge the rest back into the primary heap *)
            let (Heap (newElement, minPrimaryHeap)) =
              PrimaryHeap.findMin primaryHeap
            in let restPrimaryHeap = PrimaryHeap.deleteMin primaryHeap
            in Heap (newElement, PrimaryHeap.merge minPrimaryHeap restPrimaryHeap)
end
