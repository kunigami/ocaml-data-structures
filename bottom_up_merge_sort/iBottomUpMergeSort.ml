module type IBottomUpMergeSort = sig
  type t;;
  type sortable;;

  val empty : sortable
  (* Add an element to the structure *)
  val addElement : t -> sortable -> sortable
  (* Returns a sorted list of the elements added to the structure *)
  val sort : sortable -> t list
end
