open Parser
open Lexer
open Absyn
open Instructions
open Compiler
open Codegen

let from_string s = Parser.prog Lexer.token (Lexing.from_string s)

let compile s = compProgAndStdlib (from_string s);;

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

let sourceToSlo (filename:string) (outfile:string) =
  toFile (codeGeneration (compile (readFile filename))) outfile;
  Printf.fprintf stdout "Succesfully compiled %s to %s.\n" filename outfile
;;

let () =
  try
    match (Array.length Sys.argv) with
    | 2 -> let filename = Sys.argv.(1) in
           Printf.fprintf stdout "Filename: %s\n" filename;
           let outname = (Filename.chop_extension (Sys.argv.(1)) ^ ".slo") in
           sourceToSlo filename outname
    | 3 -> let filename = Sys.argv.(1) in
           let outname = Sys.argv.(2) in
           sourceToSlo filename outname
    | e -> Printf.fprintf stdout "Number of args: %d\n" (Array.length Sys.argv); prerr_string "Usage: slothc filename [outfile].\n";
  with e ->
    prerr_string "You need to pass a valid file path.\n";
    prerr_string "Usage: slothc filename [outfile].\n";
  exit 0
;;
