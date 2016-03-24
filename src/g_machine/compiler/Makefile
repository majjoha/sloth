all: main

shared = ../../shared
syntax = ../syntax

main:
	ocamllex ${syntax}/lexer.mll
	ocamlc -c environment.ml
	ocamlc -c ${syntax}/absyn.ml
	ocamlc -c -I ${syntax} utils.ml
	menhir --ocamlc "ocamlc -I ${syntax}" --infer -v ${syntax}/parser.mly
	ocamlc -c -I ${syntax} ${syntax}/parser.mli
	ocamlc -c -I ${syntax} ${syntax}/parser.ml
	ocamlc -c -I ${syntax} ${syntax}/lexer.ml
	ocamlc -c instructions.ml
	ocamlc -c -I ${syntax} compiler.ml
	ocamlc -c -I ${syntax} codegen.ml
	ocamlc -c -I ${syntax} ${shared}/slothc.ml
	ocamlc -I ${syntax} -I ${shared} utils.cmo environment.cmo absyn.cmo parser.cmo lexer.cmo instructions.cmo compiler.cmo codegen.cmo slothc.cmo -o slothc

clean:
	rm -f environment.cmi environment.cmo utils.cmi utils.cmo
	rm -f ${syntax}/lexer.cmi ${syntax}/lexer.cmo ${syntax}/lexer.ml
	rm -f ${syntax}/parser.automaton ${syntax}/parser.cmi ${syntax}/parser.cmo \
	 ${syntax}/parser.mli ${syntax}/parser.ml ${syntax}/parser.conflicts
	rm -f ${syntax}/absyn.cmi ${syntax}/absyn.cmo
	rm -f ${shared}/slothc.cmi ${shared}/slothc.cmo
	rm -f instructions.cmi instructions.cmo
	rm -f compiler.cmi compiler.cmo
	rm -f codegen.cmi codegen.cmo
	rm -f slothc
