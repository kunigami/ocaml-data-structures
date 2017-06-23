OBJDIR = bin
package_dependencies = -package oUnit -package batteries
test_dependencies =
compiler_flags = -linkpkg -g
stream_dependencies = -I ../stream/ ../stream/stream2.ml

all: compile
compile: scheduled_binomial_heap
scheduled_binomial_heap: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(stream_dependencies) scheduled_binomial_heap.ml
test: scheduled_binomial_heap
scheduled_binomial_heap_test: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(stream_dependencies) $(package_dependencies) \
	$(compiler_flags) $(test_dependencies) scheduled_binomial_heap.ml scheduled_binomial_heap_test.ml
	$(OBJDIR)/$@
.PHONY: scheduled_binomial_heap_test
$(OBJDIR): # Creates bin directory if it does not exist
	mkdir -p $(OBJDIR)
