

build: build.cmx
	ocamlfind ocamlopt -package xmlm,stdext -linkpkg -o build build.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package xmlm,stdext -c $<

.PHONY: build
install: build
	./build -output-dir ../dave.recoil.org/content/blog/

.PHONY: clean
clean:
	rm -f *.cmx build
