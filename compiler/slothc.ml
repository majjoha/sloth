open Parser
open Lexer
open Absyn
open Instructions
open Compiler
open Codegen

exception Mismatch

let from_string s = Parser.prog Lexer.token (Lexing.from_string s)

let compile s = compProg (from_string s);;

let toFile (instructions:int list) (file:string) =
  let oc = open_out file in
  let result =
    String.concat " " (List.map (fun i -> string_of_int(i)) instructions) in
  Printf.fprintf oc "%s\n" result;
  close_out oc;;

let example1 = 
  codeGeneration (compile "main = f 2; f x = x")
;;

let example2 = from_string "f f f f = x";;