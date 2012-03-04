

build: build.cmx
	ocamlfind ocamlopt -package xmlm,stdext -linkpkg -o build build.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,stdext -c $<

.PHONY: clean
clean:
	rm -f *.cmx build
