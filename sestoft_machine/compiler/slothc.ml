open Parser
open Lexer
open Absyn
open Instructions
open Compiler

let from_string s = Parser.prog Lexer.token (Lexing.from_string s)

let compile s = compProg (from_string s);;

let toFile (instructions:int list) (file:string) =
  let oc = open_out file in
  let result =
    String.concat " " (List.map (fun i -> string_of_int(i)) instructions) in
  Printf.fprintf oc "%s\n" result;
  close_out oc
;;

let readFile (file:string) =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)
;;
