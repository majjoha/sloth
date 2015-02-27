all: main

main:
	ocamllex lexer.mll
	ocamlc -c absyn.ml
	menhir --infer -v parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -c instructions.ml
	ocamlc -c compiler.ml
	ocamlc -c interpreter.ml

clean:
	rm lexer.cmi lexer.cmo lexer.ml
	rm parser.automaton parser.cmi parser.cmo parser.mli parser.ml parser.conflicts
	rm absyn.cmi absyn.cmo
	rm interpreter.cmi interpreter.cmo
	rm instructions.cmi instructions.cmo
	rm compiler.cmi compiler.cmo
