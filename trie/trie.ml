open BatString;;

exception Empty_string;;

module ChildList = Map.Make(Char);;
(*
  Fields:
    1. Map of chars to child nodes
    2. Whether this node represent a word
*)
type trie = Node of trie ChildList.t * bool;;

let empty: trie = Node (ChildList.empty, false);;

let rec sizeImpl (trie: trie): int =
  let Node(children, _) = trie in
    ChildList.fold
      (fun char_key node accSize -> accSize + (sizeImpl node))
      children
      (* accounts for the current node *)
      1
;;

(* Returns the number of nodes in a trie *)
let size (trie: trie): int =
  (* We need to discount one because the root node does not represent a character *)
  (sizeImpl trie) - 1
;;

(*
   Returns the number of words in a trie, or the number of nodes
   withhasEntry=true
*)
let rec entriesCount (trie: trie): int =
  let Node(children, hasEntry) = trie in
    ChildList.fold
      (fun char_key node accSize -> accSize + (entriesCount node))
      children
      (* accounts for the current node *)
      (if hasEntry then 1 else 0)
;;

let rec insert (s: char list) (trie: trie) : trie =
  let Node(children, hasEntry) = trie in match s with
    | [] -> Node(children, true)
    | first_char :: rest ->
      let currentChild = if ChildList.mem first_char children
        then (ChildList.find first_char children)
        else empty
      in
      let newChild = insert rest currentChild in
      let newChildren = ChildList.add first_char newChild children in
      Node(
        newChildren,
        hasEntry
      )
;;

let rec merge (trieA: trie) (trieB: trie): trie =
  let Node(childrenA, hasEntryA) = trieA in
  let Node(childrenB, hasEntryB) = trieB in
  let mergedChildren = ChildList.union
    (fun key nextNodeA nextNodeB -> Some (merge nextNodeA nextNodeB))
    childrenA
    childrenB
  in Node(mergedChildren, hasEntryA || hasEntryB)
;;

(* Convenience method to insert a string instead of a list of characters *)
let insertString (s: string) (trie: trie): trie =
  insert (BatString.to_list s) trie;;
;;

let rec has (s: char list) (trie: trie): bool =
  let Node(children, hasEntry) = trie in match s with
    | [] -> hasEntry
    | first_char :: rest ->
      if ChildList.mem first_char children then
        has rest (ChildList.find first_char children)
      else false
;;

let rec equals (trieA: trie) (trieB: trie): bool =
  let Node(childrenA, hasEntryA) = trieA in
  let Node(childrenB, hasEntryB) = trieB in
  if hasEntryA != hasEntryB then false
  else
    (
      ChildList.for_all (fun chr nextChildA ->
        ChildList.mem chr childrenB &&
        equals nextChildA (ChildList.find chr childrenB)
      ) childrenA
    ) &&
    (
      ChildList.for_all (fun chr nextChildB ->
        ChildList.mem chr childrenA &&
        equals nextChildB (ChildList.find chr childrenA)
      ) childrenB
    )
;;

(* Convenience method to search for a string instead of a list of characters *)
let hasString (s: string) (trie: trie): bool = has (BatString.to_list s) trie;;
