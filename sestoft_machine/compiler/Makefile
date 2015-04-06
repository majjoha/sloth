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
	ocamlc -c -I ${syntax} slothc.ml
	ocamlc -I ${shared} -I ${syntax} utils.cmo environment.cmo absyn.cmo \
		parser.cmo lexer.cmo instructions.cmo compiler.cmo codegen.cmo \
		slothc.cmo -o slothc

clean:
	rm ${shared}/environment.cmi ${shared}/environment.cmo ${shared}/utils.cmi ${shared}/utils.cmo
	rm ${shared}/syntax/lexer.cmi ${shared}/syntax/lexer.cmo ${shared}/syntax/lexer.ml
	rm ${shared}/syntax/parser.automaton ${shared}/syntax/parser.cmi ${shared}/syntax/parser.cmo \
	 ${shared}/syntax/parser.mli ${shared}/syntax/parser.ml ${shared}/syntax/parser.conflicts
	rm ${shared}/syntax/absyn.cmi ${shared}/syntax/absyn.cmo
	rm instructions.cmi instructions.cmo
	rm codegen.cmi codegen.cmo
	rm compiler.cmi compiler.cmo
	rm slothc.cmi slothc.cmo slothc
