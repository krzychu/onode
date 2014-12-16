async_tutorial.native : async_tutorial.ml
	ocamlbuild -use-ocamlfind -tag thread -pkgs core,async async_tutorial.native

unit_testing.native: unit_testing.ml
	ocamlbuild -pkgs oUnit unit_testing.native

unit_test: unit_testing.native
	./unit_testing.native

clean:
	rm -f *.native
	rm -f *.byte
	rm -rf _build
