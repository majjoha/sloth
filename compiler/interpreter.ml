open Parser
open Lexer
open Absyn
open Instructions
open Compiler

exception Mismatch

let from_string s = Parser.prog Lexer.token (Lexing.from_string s)

let compile s = compProg (from_string s);;

let toFile (instructions:int list) (file:string) =
  let oc = open_out file in
  let result =
    String.concat " " (List.map (fun i -> string_of_int(i)) instructions) in
  Printf.fprintf oc "%s\n" result;
  close_out oc;;
