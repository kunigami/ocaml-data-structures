OBJDIR = bin
package_dependencies = -package oUnit -package batteries
binary_dependencies = iDeque.ml
stream_dependencies = -I ../stream/ ../stream/stream2.ml
test_dependencies =
compiler_flags = -linkpkg -g

all: test
test: bankersDequeTest
bankersDequeTest: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
	$(stream_dependencies) $(binary_dependencies) bankersDeque.ml \
	$(test_dependencies) bankersDequeTest.ml
	$(OBJDIR)/$@
.PHONY: bankersDequeTest
clean:
	rm -f *.cmi *.cmo ./bankersDequeTest ./bankersDeque
$(OBJDIR): # Creates bin directory if it does not exist
	mkdir -p $(OBJDIR)
