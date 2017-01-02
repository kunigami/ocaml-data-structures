all: compile test
compile:
	ocamlc -I ../stream/ ../stream/stream2.ml queue_stream.ml -o queue_stream
test:
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g -I ../stream/ ../stream/stream2.ml queue_stream.ml queue_stream_test.ml
clean:
	rm -f *.cmi *.cmo ./queue_stream ./test
