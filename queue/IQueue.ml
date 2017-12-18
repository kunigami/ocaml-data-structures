(*
  Interface for all implementations of queue.
*)
module type IQueue =
 sig
   type 'a t
   val push : 'a -> 'a t -> 'a t
   val newEmpty : 'a t
   val isEmpty : 'a t -> bool
   val pop: 'a t -> 'a t
   val peek: 'a t -> 'a
 end
