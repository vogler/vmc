all:
	ocamlbuild -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom codegen.native && ./codegen.native
	# jbuilder build codegen.exe
