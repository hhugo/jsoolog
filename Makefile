
all:
	ocamlbuild -use-ocamlfind -verbose 2 -classic-display main.byte
	js_of_ocaml main.byte weak.js -noinline -pretty
