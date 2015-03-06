open Instructions
open Absyn
open Printf

type env = (string * int) list;;

type compiledSc = (string * int * instruction list);;

let failwith msg = raise (Failure msg);;

let rec inEnv (env:env) (var:string) =
  match env with
  | [] -> false
  | (x1,x2)::xs -> if x1 = var then true else inEnv xs var
;;

let printEnv (env:env) =
  List.iter (fun (s, i) -> fprintf stdout ("Name: %s, Index: %d\n") s i) env
;;

let rec lookup (env:env) (var:string) =
  match env with
  | [] -> failwith "Variable was not in environment."
  | (x1,x2)::xs -> if x1 = var then x2 else lookup xs var
;;

let argOffset (env:env) (n:int) =
  List.map (fun (v,m) -> (v,m+n)) env
;;

let rec compC (expr:expr) (env:env) =
  match expr with
  | Var s -> if inEnv env s then [Push (lookup env s)] else [Pushglobal s]
  | Num n -> [Pushint n]
  | App (e1, e2) -> compC e2 env @ compC e1 (argOffset env 1) @ [Mkap]
  | Let (defns, body) -> 
      let compDefns = List.flatten (List.mapi (fun n (s, e) -> compC e (argOffset env n)) defns) in
      let newEnv = (List.mapi (fun n (s, e) -> (s, n)) (List.rev defns)) @ argOffset env (List.length defns) in
      compDefns @ (compC body newEnv) @ [Slide (List.length defns)]
  | _ -> failwith "Unimplemented expression type."
;;

let rec compR (expr:expr) (env:env) =
  let n = List.length env in
  compC expr env @ [Update n; Pop n; Unwind]
;;

let rec compSc (sc:scdefn) : compiledSc = 
  match sc with
  | (name, args, body) ->
      (name, List.length args,
        compR body (List.mapi (fun i a -> (a, i)) args))
;;

let rec compProg (prog:program) =
  match prog with
  | [] -> []
  | x::xs -> compSc x :: compProg xs
;;
