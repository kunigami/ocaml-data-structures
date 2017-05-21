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

let testComparingTries text_ctx =
  let trieA = Trie.(empty |> insertString "abc" |> insertString "bc") in
  let trieB = Trie.(empty |> insertString "bc" |> insertString "abc") in
  assert_bool "Insertion order does not matter" (Trie.equals trieA trieB)

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

let testSize text_ctx =
  let sampleTrie = Trie.(empty |> insertString "abc" |> insertString "ab" |> insertString "a") in
  assert_equal ~msg:"It should contain 3 characters" (Trie.size sampleTrie) 3

let testEntriesCount text_ctx =
  let sampleTrie = Trie.(
    empty |>
    insertString "abc" |>
    insertString "ab" |>
    insertString "a" |>
    (* Trie should  include repeated entries only once *)
    insertString "a" |>
    insertString "bc" |>
    insertString "bc"
  ) in
  assert_equal
    ~msg:"It should contain 4 unique entries"
    (Trie.entriesCount sampleTrie)
    4
;;

let testMergingTries text_ctx =
  let trieA = Trie.(empty |> insertString "abc" |> insertString "ab" |> insertString "c") in
  let trieB = Trie.(empty |> insertString "ab" |> insertString "cd" |> insertString "d") in
  let mergedTrie = Trie.merge trieA trieB in
  let expectedTrie = Trie.(
    empty |>
    insertString "ab" |>
    insertString "abc" |>
    insertString "c" |>
    insertString "cd" |>
    insertString "d"
  ) in
  assert_bool
    "Merging two tries is equivalent to a trie with the words from both tries"
    (Trie.equals mergedTrie expectedTrie)
;;

let suite =
"suite">:::
 [
  "testQueryOnEmptyTrie">:: testQueryOnEmptyTrie;
  "testComparingTries">:: testComparingTries;
  "testSimpleInsertionAndQuery">:: testSimpleInsertionAndQuery;
  "testPrefixes">:: testPrefixes;
  "testPrefixesReversed">:: testPrefixesReversed;
  "testPrefixes2">:: testPrefixes2;
  "testSameLetter">:: testSameLetter;
  "testSize">:: testSize;
  "testEntriesCount">:: testEntriesCount;
  "testMergingTries">:: testMergingTries;
 ]
;;

let () =
  run_test_tt_main suite
;;
