all: main

shared = ../../shared
syntax = ${shared}/syntax

main:
	ocamllex ${shared}/syntax/lexer.mll
	ocamlc -c ${shared}/environment.ml
	ocamlc -c ${shared}/utils.ml
	ocamlc -c ${shared}/syntax/absyn.ml
	menhir --ocamlc "ocamlc -I ${shared}/syntax" --infer -v ${shared}/syntax/parser.mly
	ocamlc -c -I ${syntax} ${shared}/syntax/parser.mli
	ocamlc -c -I ${syntax} ${shared}/syntax/parser.ml
	ocamlc -c -I ${syntax} ${shared}/syntax/lexer.ml
	ocamlc -c instructions.ml
	ocamlc -c -I ${shared} -I ${syntax} compiler.ml
	ocamlc -c -I ${shared} codegen.ml
	ocamlc -c -I ${syntax} ${shared}/slothc.ml
	ocamlc -I ${shared} -I ${syntax} utils.cmo environment.cmo absyn.cmo \
		parser.cmo lexer.cmo instructions.cmo compiler.cmo codegen.cmo \
		slothc.cmo -o slothc

clean:
	rm -f ${shared}/environment.cmi ${shared}/environment.cmo ${shared}/utils.cmi ${shared}/utils.cmo
	rm -f ${shared}/syntax/lexer.cmi ${shared}/syntax/lexer.cmo ${shared}/syntax/lexer.ml
	rm -f ${shared}/syntax/parser.automaton ${shared}/syntax/parser.cmi ${shared}/syntax/parser.cmo \
	 ${shared}/syntax/parser.mli ${shared}/syntax/parser.ml ${shared}/syntax/parser.conflicts
	rm -f ${shared}/syntax/absyn.cmi ${shared}/syntax/absyn.cmo
	rm -f ${shared}/slothc.cmi ${shared}/slothc.cmo
	rm -f instructions.cmi instructions.cmo
	rm -f codegen.cmi codegen.cmo
	rm -f compiler.cmi compiler.cmo
	rm -f slothc
