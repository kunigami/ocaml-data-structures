OBJDIR = bin
package_dependencies = -package oUnit -package batteries
test_dependencies = iHeap.ml heapTestBase.ml
compiler_flags = -linkpkg -g
stream_dependencies = -I ../stream/ ../stream/stream2.ml

all: compile
compile: scheduled_binomial_heap
scheduled_binomial_heap: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(stream_dependencies) scheduled_binomial_heap.ml
test: binomial_heap_test scheduled_binomial_heap skewBinomialHeapTest
binomial_heap_test: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(stream_dependencies) $(package_dependencies) \
	$(compiler_flags) $(test_dependencies) binomial_heap.ml $@.ml
	$(OBJDIR)/$@
scheduled_binomial_heap_test: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(stream_dependencies) $(package_dependencies) \
	$(compiler_flags) $(test_dependencies) scheduled_binomial_heap.ml $@.ml
	$(OBJDIR)/$@
skewBinomialHeapTest: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(stream_dependencies) $(package_dependencies) \
	$(compiler_flags) $(test_dependencies) skewBinomialHeap.ml $@.ml
	$(OBJDIR)/$@
leftistHeapTest: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(stream_dependencies) $(package_dependencies) \
	$(compiler_flags) $(test_dependencies) leftistHeap.ml $@.ml
	$(OBJDIR)/$@
.PHONY: scheduled_binomial_heap_test skewBinomialHeapTest
$(OBJDIR): # Creates bin directory if it does not exist
	mkdir -p $(OBJDIR)
