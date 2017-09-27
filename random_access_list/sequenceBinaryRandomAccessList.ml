open IRandomAccessList;;

(*

  A sequence is a recursive non-uniform data structure. It's similar to a list,
  but item following the current one holds twice as many elements. For example,
  the first item has 1 element, the second 2, the third 4 and so on. This is
  equivalent to a complete binary tree.

  Example:

    cons(1, cons((2, 3), cons(()(4, 5), (6, 7)), nil)))

  The problem with this representation is that it can only represent number in
  the form of 2^k - 1. To work around that, we allow some items to not hold any
  elements at all. We can now represent 10, as

    zero(one((1, 2), zero(one(((3, 4), (5, 6)), ((7, 8),(9, 10))), nil))))

  Adding an element to the front of the sequence is equivalent to incrementing
  the binary digit. Zero becomes One, and One must be carried over.

  Removing an element from the front is equivalent to decrementing the binary
  digit.

  For searching, the index transformation is harder to grasp. Let's consider a
  example where we have all elements in all digits, say for N=7. Index-wise, the
  sequence looks like:

  l1: 0
  l2: (1, 2)
  l3: ((3, 4), (5, 6))

  If our search index is 0, we stop at the first level. Otherwise, we decrement
  the index by one. The tree looks like this now:

  l1: -
  l2: (0, 1)
  l3: ((2, 3), (4, 5))

  Now we go down one level but we need to divide the index by two, because we'll
  be returning an element of twice the size from the next level.

  The tree looks like this now:

  l2: (0, 0)
  l3: ((1, 1), (2, 2))

  Again, if the index is 0, we return right away. But we'll be returning a pair,
  so we need to decide which element from the pair to use. Before dividing by 2,
  the index for the pair was (0, 1), so we can use index mod 2 to decide which
  one to pick.
*)

module SequenceBinaryRandomAccessList: IRandomAccessList =
  struct
    exception IndexOutOfBoundsException;;
    exception EmptyListException;;
    exception ViolationException;;

    type 'a seq = Nil | Zero of ('a * 'a) seq | One of 'a * ('a * 'a) seq;;
    type 'a t = 'a seq;;

    let empty = Nil

    let isEmpty digits = match digits with
      | Nil -> true
      | _ -> false

    let rec push: 'a. 'a -> 'a seq -> 'a seq =
      fun element digits -> match digits with
        | Nil -> One (element, Nil)
        | Zero restDigits ->  One (element, restDigits)
        | One (currentElement, restDigits) ->
          (*
            Combine the current element and the one to insert into one, for a
            type ('a*'a), which is what the recursive step expects.
          *)
          Zero (push (element, currentElement) restDigits)
    ;;

    let rec popAux: 'a. 'a seq -> ('a * 'a seq) = function
      | Nil -> raise EmptyListException
      | One (elem, Nil) -> (elem, Nil)
      | One (elem, restDigits) -> (elem, Zero restDigits)
      | Zero (restDigits) ->
        (*
          Splits the element returned ('a * 'a) into two, returning the first
          one, and making the right one the contents of the current digit.
        *)
        let ((left, right), restResult) = popAux restDigits in
        (left, One (right, restResult))

    let head digits = let (elem, _) = popAux digits in elem;;
    let tail digits = let (_, newDigits) = popAux digits in newDigits;;

    let rec lookup: 'a. int -> 'a seq -> 'a =
      fun index digits -> match (index, digits) with
        | (index, Nil) -> raise IndexOutOfBoundsException
        | (0, One (elem, restDigits)) -> elem
        (* Have to wrap in Zero because we're not really changing the level. The
           next step in the recursion will be handled by the next case *)
        | (index, One (_, restDigits)) -> lookup (index - 1) (Zero restDigits)
        (* We have to transform the index as described at the top of the class *)
        | (index, Zero restDigits) ->
          let (left, right) = lookup (index / 2) restDigits
          in if index mod 2 == 0 then left else right

    let rec fupdate: 'a. ('a -> 'a) -> int -> 'a seq -> 'a seq =
      fun updater index digits -> match (index, digits) with
        | (index, Nil) -> raise IndexOutOfBoundsException
        | (0, One (elem, restDigits)) -> One (updater elem, restDigits)
        | (index, One (elem, restDigits)) ->
          push elem (fupdate updater (index - 1) (Zero restDigits))
        | (index, Zero restDigits) ->
          (* For each level we go down, we add a layer of updater. It capture
            the path we took to get to the element when the index is finally 0
          *)
          let updater' = fun (x, y) -> if index mod 2 == 0
            then (updater x, y)
            else (x, updater y)
          in Zero (fupdate updater' (index / 2) restDigits)

    let rec update index element digits =
      fupdate (fun x -> element) index digits
    ;;
  end
;;
