<<<<<<< HEAD
all: evaluation expr miniml tests

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

clean:
=======
all: evaluation expr miniml tests

mini: evaluation expr miniml

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

clean:
>>>>>>> untested
	rm -rf _build *.byte