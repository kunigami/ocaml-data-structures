all: compile test
compile:
	ocamlc -I ../stream/ ../stream/stream2.ml queue_stream.ml -o queue_stream
test: test_stream_queue test_queue test_suspended_queue
test_stream_queue:
	ocamlfind ocamlc -o queue_stream_test -package oUnit -linkpkg -g -I ../stream/ ../stream/stream2.ml queue_stream.ml queue_stream_test.ml
	./queue_stream_test
test_queue:
	ocamlfind ocamlc -o queue2_test -package oUnit -linkpkg -g queue2.ml queue2_test.ml
	./queue2_test
test_suspended_queue:
	ocamlfind ocamlc -o queue_suspended_test -package oUnit -linkpkg -g queue_suspended.ml queue_suspended_test.ml
	./queue_suspended_test
test_real_time_queue:
	ocamlfind ocamlc -o real_time_queue_test -package oUnit -package batteries -linkpkg -g -I ../stream/ ../stream/stream2.ml real_time_queue.ml real_time_queue_test.ml
	./real_time_queue_test
clean:
	rm -f *.cmi *.cmo ./queue_stream ./queue2_test ./queue_stream_test ./queue_suspension_test ./real_time_queue_test
