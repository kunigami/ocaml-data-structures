(*
  Interface for a random access list.
*)
module type IRandomAccessList =
 sig
   type 'a t

   val empty: 'a t;;
   val isEmpty: 'a t -> bool;;

   val push: 'a -> 'a t -> 'a t;;
   val head: 'a t -> 'a;;
   val tail: 'a t -> 'a t;;

   val lookup: int -> 'a t -> 'a;;
   val update: int -> 'a -> 'a t -> 'a t;; 

 end;;
