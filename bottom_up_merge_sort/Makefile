OBJDIR = bin
package_dependencies = -package oUnit -package batteries
test_dependencies = iBottomUpMergeSort.ml
compiler_flags = -linkpkg -g
stream_dependencies = -I ../stream/ ../stream/stream2.ml

all: compile
test: scheduledBottomUpMergeSortTest
scheduledBottomUpMergeSortTest: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(stream_dependencies) $(package_dependencies) \
	$(compiler_flags) $(test_dependencies) scheduledBottomUpMergeSort.ml $@.ml
	$(OBJDIR)/$@
.PHONY: scheduledBottomUpMergeSortTest
$(OBJDIR): # Creates bin directory if it does not exist
	mkdir -p $(OBJDIR)
