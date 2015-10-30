all: i18n

i18n: i18n.ml
	ocamlfind ocamlopt -package csv,str -linkpkg -o $@ $^
