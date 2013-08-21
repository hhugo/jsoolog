
.PHONY: exemple

all: lib

setup.data:
	oasis setup

configure: setup.data
	ocaml setup.ml -configure

lib: configure
	ocaml setup.ml -build

exemple:
	ocaml setup.ml -build exemple/main.byte
	js_of_ocaml _build/exemple/main.byte +weak.js -noinline -pretty

clean:
	ocaml setup.ml -clean
