open Batteries;;

exception Empty_string;;

module ChildList = Map.Make(Char);;
(*
  Fields:
    1. Map of chars to child nodes
    2. Whether this node represent a word
*)
type trie = Node of trie ChildList.t * bool;;

let empty: trie = Node (ChildList.empty, false);;

let rec insert (s: char list) (trie: trie) : trie =
  let Node(children, isLeaf) = trie in match s with
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
        isLeaf
      )

(* Convenience method to insert a string instead of a list of characters *)
let insertString (s: string) (trie: trie): trie =
  insert (String.to_list s) trie;;

let rec has (s: char list) (trie: trie): bool =
  let Node(children, isLeaf) = trie in match s with
    | [] -> isLeaf
    | first_char :: rest ->
      if ChildList.mem first_char children then
        has rest (ChildList.find first_char children)
      else false

(* Convenience method to search for a string instead of a list of characters *)
let hasString (s: string) (trie: trie): bool = has (String.to_list s) trie;;
