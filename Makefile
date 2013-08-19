
all:
	ocamlbuild -use-ocamlfind -verbose 2 -classic-display log.byte
	js_of_ocaml log.byte weak.js -noinline -pretty
