OBJDIR = bin
package_dependencies = -package oUnit -package batteries
compiler = ocamlfind ocamlc
compiler_flags = -linkpkg -g
stream_dependencies = -I ../stream/ ../stream/stream2.ml
queue_dependencies = -I ../queue/ ../queue/IQueue.ml
deque_dependencies = -I ../deque/ ../deque/iDeque.ml ../deque/bankersDeque.ml \
	../deque/dequeTestBase.ml
test_dependencies = iCatenableDeque.ml catenableDequeBaseTest.ml

all: test
test: simpleCatenableDequeTest implicitCatenableDequeTest
simpleCatenableDequeTest: | $(OBJDIR)
	$(compiler) -o $(OBJDIR)/$@ $(package_dependencies) \
	$(compiler_flags) $(stream_dependencies) $(deque_dependencies) \
	$(test_dependencies) simpleCatenableDeque.ml $@.ml
	$(OBJDIR)/$@
implicitCatenableDequeTest: | $(OBJDIR)
	$(compiler) -o $(OBJDIR)/$@ $(package_dependencies) \
	$(compiler_flags) $(stream_dependencies) $(deque_dependencies) \
	$(test_dependencies) implicitCatenableDeque.ml $@.ml
	$(OBJDIR)/$@

clean:
	rm -f $(OBJDIR)/*

.PHONY: simpleCatenableDequeTest implicitCatenableDequeTest
$(OBJDIR): # Creates bin directory if it does not exist
	mkdir -p $(OBJDIR)
