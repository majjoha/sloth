open Instructions
open Absyn
open Printf
open Environment
open Utils

type compiledSc = (string * int * instruction list);;

let rec compC (expr:expr) (env:env) : instruction list =
  match expr with
  | App(e1, e2) -> 
    (match e2 with
    | Var s -> if inEnv env s then Push (lookup env s) :: compC e2 env else 
    | _     -> failwith "Function applied to non-variable expression")
  | Var s       -> if inEnv env s then [Enter (lookup env s)] else 
  | Letrec (defns, body) | Let (defns, body) -> 
    let n = List.length defns in
    let newEnv = (List.mapi (fun i (s, e) -> (s, i)) (List.rev defns)) @ argOffset env n in
    Let n
    :: (List.fold_right (fun (s, e) acc -> (compC e newEnv) @ (Sep :: acc)) defns []
        @ compC body newEnv)
  | _ -> failwith "Unsupported expression in abstract syntax tree"
;;

let rec compSc (sc:scdefn) : instruction list =
  let (name, args, body) = sc in
  Take :: compC body (List.mapi (fun i a -> (a, i)) args)
;;

let rec compProg (prog:program) =
  match prog with
  | [] -> []
  | x::xs -> compSc x :: compProg xs
;;