OBJDIR = bin
package_dependencies = -package oUnit -package batteries
binary_dependencies = IRandomAccessList.ml
stream_dependencies = -I ../stream/ ../stream/stream2.ml
test_dependencies = randomAccessListTestBase.ml
compiler_flags = -linkpkg -g

all: test
test: binaryRandomAccessListTest
binaryRandomAccessListTest: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
		$(binary_dependencies) binaryRandomAccessList.ml \
		$(test_dependencies) binaryRandomAccessListTest.ml
	$(OBJDIR)/$@

.PHONY: random_access_list_test
clean:
	rm -f *.cmi *.cmo ./binaryRandomAccessListTest
$(OBJDIR): # Creates bin directory if it does not exist
	mkdir -p $(OBJDIR)
