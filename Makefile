build:
	jbuilder build vmc.exe && \
	cp _build/default/vmc.exe vmc

run:
	jbuilder build @install && \
	jbuilder exec vmc

test: # regression tests
	jbuilder runtest --diff-command='diff -u10 --color=always'

unit: # debug/unit tests
	ocamlbuild -no-links -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom tests/test.native && \
	./_build/tests/test.native

ocamlbuild: # just here for comparison
	ocamlbuild -no-links -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom vmc.native && \
	cp _build/vmc.native vmc

clean:
	jbuilder clean
	ocamlbuild -clean
	rm -f vmc

.PHONY: build run test unit ocamlbuild clean
