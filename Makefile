all:
	ocamlbuild -use-ocamlfind -package batteries -package ppx_deriving.std -package angstrom vmc.native && ./vmc.native

# all: # use this once ppx_deriving etc. work with jbuilder...
	# jbuilder build codegen.exe
