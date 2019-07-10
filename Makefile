build:
	dune build vmc.exe && \
	cp _build/default/vmc.exe vmc

run:
	dune build @install && \
	dune exec vmc

test: # regression tests
	dune runtest --diff-command='diff -u10 --color=always'

unit: # debug/unit tests
	ocamlbuild -no-links -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom tests/test.native && \
	./_build/tests/test.native

ocamlbuild: # just here for comparison
	ocamlbuild -no-links -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom vmc.native && \
	cp _build/vmc.native vmc

clean:
	dune clean
	ocamlbuild -clean
	rm -f vmc

.PHONY: build run test unit ocamlbuild clean
