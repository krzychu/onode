async:
	ocamlbuild -lib unix async.native

select:
	ocamlbuild -lib unix select.native

async_tutorial:
	ocamlbuild -use-ocamlfind -tag thread -pkgs core,async async_tutorial.native

unit_testing:
	ocamlbuild -pkgs oUnit unit_testing.native

unit_test: unit_testing
	./unit_testing.native

clean:
	rm -f *.native
	rm -f *.byte
	rm -rf _build
