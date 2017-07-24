# all:
# 	ocamlbuild -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom vmc.native && ./vmc.native

all:
	jbuilder build vmc.exe && ./_build/default/vmc.exe

test:
	# jbuilder runtest
	# TODO jbuild for test
	ocamlbuild -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom tests/test.native && ./test.native

clean:
	jbuilder clean
	ocamlbuild -clean
