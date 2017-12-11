open Map;;

(*
  Implementation of a trie over a tree. The usual implementation of tries
  are over strings keys, but they can be generalized to take any structures such as
  trees. We can think of tries as key-value structures.

  We could reduce the tree as key to a string by seriaziling it in an unique
  way. This implementation doesn't require that by relying on nested
  tries.

  An analogy is a Map over a tuple of keys. If we have a triple for example,
  (key1, key2, key3), we could have nested maps such as
  map<key1, map<key2, map<key3, 'a>>>. (NOTE: I'm using a notation that is
  reversed from OCaml's - map<int, map<int, 'a>> vs. (int (int 'a map) map) -
  because I find it easier to read)

  Now say we have a tree of integers. The type can be defined recursively as
  triple: tree<int> = (int, tree<int>, tree<int>). We could have a map using a
  tree as key. The type of such map would be map<tree<int>, 'a> . We can then
  expand the recusive call to get 3 nested maps:

    map<int, map<tree<int>, map<tree<int>, 'a>>>>

  The first look up is over the outermost map. If the key exists in there,
  we get this following map

    map<tree<int>, map<tree<int>, 'a>>>

  The key is a recursive type again, so we need to expand. This time to

    map<int, map<tree<int>, map<tree<int>, 'b>>>>

  where 'b =  map<tree<int>, 'a>>. The type gets pretty involved very quickly,
  but if we work with these types recursively the code is not too complicated.

  Note that this requires the use of non-uniform polymorphic recursive type
  (https://kunigami.blog/2017/10/02/polymorphic-recursion-in-ocaml/)
*)
module TrieOverTree(M: Map.S) =
  struct

    type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
    type key = M.key tree

    type 'a trie = Trie of 'a option * 'a trie trie M.t

    let empty = Trie (None, M.empty)

    let rec find: 'a. key -> 'a trie -> 'a =
      fun tree trie -> match (tree, trie) with
        | (Empty, Trie (None, children)) -> raise Not_found
        | (Empty, Trie (Some value, children)) -> value
        | (Node (root, left, right), Trie (_, children)) ->
          let trieForRoot = M.find root children in
          let trieForLeft = find left trieForRoot in
          find right trieForLeft
    ;;

    let rec insert: 'a. key -> 'a -> 'a trie -> 'a trie =
      fun tree value trie -> match (tree, trie) with
        | (Empty, Trie (_, children)) -> Trie (Some value, children)
        | (Node (root, left, right), Trie (trieValue, children)) ->
          let trieForRoot = try
            M.find root children
          with
            Not_found -> empty
          in
          let trieForLeft = try
            find left trieForRoot
          with
            Not_found -> empty
          in
          let newTrieForLeft = insert right value trieForLeft in
          let newTrieForRoot = insert left newTrieForLeft trieForRoot in
          let newChildren = M.add root newTrieForRoot children in
          Trie (trieValue, newChildren)
    ;;

    let has tree trie =
      try
        let () = find tree trie
        in true
      with
        Not_found -> false

end
