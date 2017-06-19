OBJDIR = bin
package_dependencies = -package oUnit -package batteries
test_dependencies = test_base.ml
compiler_flags = -linkpkg -g

all: compile test
compile:
	ocamlc -I ../stream/ ../stream/stream2.ml queue_stream.ml -o queue_stream
test: queue_stream_test queue2_test queue_suspended_test real_time_queue_test
queue_stream_test: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) -I ../stream/ ../stream/stream2.ml iQueue.ml queue_stream.ml test_base.ml queue_stream_test.ml
	$(OBJDIR)/$@
queue2_test: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) iQueue.ml queue2.ml $(test_dependencies) queue2_test.ml
	$(OBJDIR)/$@
queue_suspended_test: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) iQueue.ml queue_suspended.ml $(test_dependencies) queue_suspended_test.ml
	$(OBJDIR)/$@
real_time_queue_test: | $(OBJDIR)
	ocamlfind ocamlc -o $(OBJDIR)/$@ $(package_dependencies) $(compiler_flags) -I ../stream/ ../stream/stream2.ml iQueue.ml real_time_queue.ml $(test_dependencies) real_time_queue_test.ml
	$(OBJDIR)/$@
.PHONY: queue_stream_test queue2_test queue_suspended_test real_time_queue_test
clean:
	rm -f *.cmi *.cmo ./queue_stream ./queue2_test ./queue_stream_test ./queue_suspended_test ./real_time_queue_test
$(OBJDIR): # Creates bin directory if it does not exist
	mkdir -p $(OBJDIR)
