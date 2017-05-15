all: test
test: test_trie
test_trie:
	ocamlfind ocamlc -o trie_test -package oUnit -package batteries -linkpkg -g trie.ml trie_test.ml
	./trie_test
clean:
	rm -f *.cmi *.cmo ./trie_test
