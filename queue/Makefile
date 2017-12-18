OBJDIR = bin
package_dependencies = -package oUnit -package batteries
binary_dependencies = IQueue.ml exceptions.ml
stream_dependencies = -I ../stream/ ../stream/stream2.ml
test_dependencies = makeQueueUtils.ml test_base.ml
compiler_flags = -linkpkg -g
compiler = ocamlfind ocamlc
shared = $(compiler) -o $(OBJDIR)/$@ $(package_dependencies) \
	$(binary_dependencies) $(compiler_flags) $(test_dependencies)

all: compile test
compile:
	ocamlc -I ../stream/ ../stream/stream2.ml queue_stream.ml -o queue_stream
test: queue_stream_test queue2_test queue_suspended_test real_time_queue_test \
	hood_meville_queue_test implicitQueueTest
queue_stream_test: | $(OBJDIR)
	$(compiler) -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
		$(stream_dependencies) $(binary_dependencies) queue_stream.ml \
		$(test_dependencies) queue_stream_test.ml
	$(OBJDIR)/$@
queue2_test: | $(OBJDIR)
	$(compiler) -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
		$(binary_dependencies) queue2.ml $(test_dependencies) queue2_test.ml
	$(OBJDIR)/$@
queue_suspended_test: | $(OBJDIR)
	$(compiler) -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
	 	$(binary_dependencies) queue_suspended.ml $(test_dependencies) \
		queue_suspended_test.ml
	$(OBJDIR)/$@
real_time_queue_test: | $(OBJDIR)
	$(compiler) -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
		$(stream_dependencies) $(binary_dependencies) real_time_queue.ml \
		$(test_dependencies) real_time_queue_test.ml
	$(OBJDIR)/$@
hood_meville_queue_test: | $(OBJDIR)
	$(compiler) -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) \
		$(stream_dependencies) $(binary_dependencies) hoodMevilleQueue.ml \
		$(test_dependencies) hoodMevilleQueueTest.ml
	$(OBJDIR)/$@
implicitQueueTest: | $(OBJDIR)
	$(shared) implicitQueue.ml  implicitQueueTest.ml
	$(OBJDIR)/$@

.PHONY: queue_stream_test queue2_test queue_suspended_test \
	real_time_queue_test hood_meville_queue_test implicitQueueTest
clean:
	rm -f *.cmi *.cmo ./queue_stream ./queue2_test ./queue_stream_test ./queue_suspended_test ./real_time_queue_test
$(OBJDIR): # Creates bin directory if it does not exist
	mkdir -p $(OBJDIR)
