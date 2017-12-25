open IDeque
(*
  Interface for all implementations of a catenable dequeue.
*)
module type ICatenableDeque =
 sig

   include IDeque
   val concat: 'a t -> 'a t -> 'a t
 end
