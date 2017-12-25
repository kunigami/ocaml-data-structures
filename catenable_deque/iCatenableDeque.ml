(*
  Interface for all implementations of a catenable dequeue.
  TODO: include IDeque
*)
module type ICatenableDeque =
 sig
   type 'a t

   val newEmpty : 'a t

   val isEmpty: 'a t -> bool

   (* Insert an element at the front *)
   val pushFront: 'a -> 'a t -> 'a t
   (* Retrieve element from the front *)
   val peekFront: 'a t -> 'a
   (* Remove element from the front *)
   val popFront: 'a t -> 'a t

   (* Insert an element at the rear *)
   val pushBack : 'a -> 'a t -> 'a t
   (* Remove element from the rear *)
   val popBack: 'a t -> 'a t
   (* Retrieve element from the rear *)
   val peekBack: 'a t -> 'a

   val concat: 'a t -> 'a t -> 'a t
 end
