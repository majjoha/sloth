open Parser
open Lexer
open Absyn

exception Mismatch

let from_string s = Parser.main Lexer.main (Lexing.from_string s)

let rec eval (e:expr option) =
  match e with
  | Some Prim("+", e1, e2) -> (eval (Some e1)) + (eval (Some e2))
  | Some Prim("-", e1, e2) -> (eval (Some e1)) - (eval (Some e2))
  | Some CstI i            -> i
  | _                      -> raise Mismatch
;
