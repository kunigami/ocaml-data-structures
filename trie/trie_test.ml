open OUnit2;;
open Trie;;

(* -----------------------------------------------------------------------------
 * Helper functions
 * -----------------------------------------------------------------------------
 *)

let testQueryOnEmptyTrie text_ctx =
  let emptyTrie = Trie.empty in
  assert_bool "Should not contain any string" (not (Trie.hasString "a" emptyTrie))
;;

let testSimpleInsertionAndQuery text_ctx =
  let sampleTrie = Trie.(empty |> insertString "abc" |> insertString "abd") in
  let () = assert_bool "Should contain string abc" (Trie.hasString "abc" sampleTrie) in
  let () = assert_bool "Should contain string abd" (Trie.hasString "abd" sampleTrie) in
  let () = assert_bool "Should not contain string abe" (not (Trie.hasString "abe" sampleTrie)) in
  let () = assert_bool "Should not contain substrings" (not (Trie.hasString "ab" sampleTrie)) in
  ()
;;

let testPrefixes text_ctx =
  let sampleTrie = Trie.(empty |> insertString "abc" |> insertString "ab" |> insertString "a") in
  let () = assert_bool "Should contain string abc" (Trie.hasString "abc" sampleTrie) in
  let () = assert_bool "Should contain string ab" (Trie.hasString "ab" sampleTrie) in
  let () = assert_bool "Should contain string a" (Trie.hasString "a" sampleTrie) in
  ()
;;

(* Inserting the prefixes from the smallest to the largest exercies a different code path *)
let testPrefixesReversed text_ctx =
  let sampleTrie = Trie.(empty |> insertString "a" |> insertString "ab" |> insertString "abc") in
  let () = assert_bool "Should contain string abc" (Trie.hasString "abc" sampleTrie) in
  let () = assert_bool "Should contain string ab" (Trie.hasString "ab" sampleTrie) in
  let () = assert_bool "Should contain string a" (Trie.hasString "a" sampleTrie) in
  ()
;;

let testPrefixes2 text_ctx =
  let sampleTrie = Trie.(empty |> insertString "abc" |> insertString "a") in
  let () = assert_bool "Should contain string abc" (Trie.hasString "abc" sampleTrie) in
  let () = assert_bool "Should contain string ab" (not (Trie.hasString "ab" sampleTrie)) in
  let () = assert_bool "Should contain string a" (Trie.hasString "a" sampleTrie) in
  ()
;;

let testSameLetter text_ctx =
  let sampleTrie = Trie.(empty |> insertString "aaaaaaa" |> insertString "a") in
  let () = assert_bool "Should contain string aaaaaaa" (Trie.hasString "aaaaaaa" sampleTrie) in
  let () = assert_bool "Should contain string aaaa" (not (Trie.hasString "aaaa" sampleTrie)) in
  let () = assert_bool "Should contain string a" (Trie.hasString "a" sampleTrie) in
  ()
;;

let suite =
"suite">:::
 [
  "testQueryOnEmptyTrie">:: testQueryOnEmptyTrie;
  "testSimpleInsertionAndQuery">:: testSimpleInsertionAndQuery;
  "testPrefixes">:: testPrefixes;
  "testPrefixesReversed">:: testPrefixesReversed;
  "testPrefixes2">:: testPrefixes2;
  "testSameLetter">:: testSameLetter;
 ]
;;

let () =
  run_test_tt_main suite
;;
