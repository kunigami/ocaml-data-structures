all: test
test:
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g splay_tree.ml splay_tree_test.ml
clean:
	rm -f splay_tree_test.cmi splay_tree_test.cmo test
