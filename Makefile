build:
	jbuilder build vmc.exe
	cp _build/default/vmc.exe vmc

# run:
	# jbuilder exec vmc # why doesn't this work?

test: # regression tests
	jbuilder runtest

unit: # debug/unit tests
	ocamlbuild -no-links -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom tests/test.native && ./_build/tests/test.native

ocamlbuild: # just here for comparison
	ocamlbuild -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom vmc.native

clean:
	jbuilder clean
	ocamlbuild -clean
	rm -f vmc

.PHONY: build test unit ocamlbuild clean
