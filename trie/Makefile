OBJDIR = bin
package_dependencies = -package oUnit -package batteries
test_dependencies =
compiler_flags = -linkpkg -g

all: test
test: trie_test trieOverTreeTest
trie_test: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
	$(test_dependencies) trie.ml trie_test.ml
	$(OBJDIR)/$@
trieOverTreeTest: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
	$(test_dependencies) trieOverTree.ml trieOverTreeTest.ml
	$(OBJDIR)/$@
clean:
	rm -f *.cmi *.cmo bin/*
