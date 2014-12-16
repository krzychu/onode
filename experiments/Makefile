all: test

unit_testing.native: unit_testing.ml
	ocamlbuild -pkgs oUnit unit_testing.native

test: unit_testing.native
	./unit_testing.native

clean:
	rm -f *.native
	rm -f *.byte
	rm -rf _build
