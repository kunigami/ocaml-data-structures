package_dependencies = -package oUnit -package batteries
test_dependencies = test_base.ml
compiler_flags = -linkpkg -g

all: compile test
compile:
	ocamlc -I ../stream/ ../stream/stream2.ml queue_stream.ml -o queue_stream
test: queue_stream_test queue2_test queue_suspended_test real_time_queue_test
queue_stream_test:
	ocamlfind ocamlc -o $@ $(package_dependencies) $(compiler_flags) -I ../stream/ ../stream/stream2.ml iQueue.ml queue_stream.ml test_base.ml queue_stream_test.ml
	./$@
queue2_test:
	ocamlfind ocamlc -o $@ $(package_dependencies) $(compiler_flags) iQueue.ml queue2.ml $(test_dependencies) queue2_test.ml
	./$@
queue_suspended_test:
	ocamlfind ocamlc -o $@ $(package_dependencies) $(compiler_flags) iQueue.ml queue_suspended.ml $(test_dependencies) queue_suspended_test.ml
	./$@
real_time_queue_test:
	ocamlfind ocamlc -o $@ $(package_dependencies) $(compiler_flags) -I ../stream/ ../stream/stream2.ml iQueue.ml real_time_queue.ml $(test_dependencies) real_time_queue_test.ml
	./$@
.PHONY: queue_stream_test queue2_test queue_suspended_test real_time_queue_test
clean:
	rm -f *.cmi *.cmo ./queue_stream ./queue2_test ./queue_stream_test ./queue_suspension_test ./real_time_queue_test
