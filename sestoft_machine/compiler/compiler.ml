open Instructions
open Absyn
open Printf
open Environment
open Utils
open Preprocessor

type compiledSc = (string * int * instruction list);;

let rec compFvs (vars:string list) (env:env) =
  Freevars (List.map (fun s -> lookup env s) vars)

and compDefns (defns:(string * expr) list) (env:env) =
  List.fold_right (fun (s, e) acc -> (compFvs (findFreeVars e []) env) ::
                                     (compC e env) @
                                     (Sep :: acc)) defns []

and compC (expr:expr) (env:env) : instruction list =
  match expr with
  | App(e1, e2) -> 
    (match e2 with
    | Var s -> Push (lookup env s) :: compC e1 env
    | _     -> failwith "Function applied to non-variable expression")
  | Var s -> Printf.fprintf stdout "%s\n" s; [Enter (lookup env s)]
  | Letrec (defns, body) | Let (defns, body) -> 
    let n = List.length defns in
    let newEnv = (List.mapi (fun i (s, e) -> (s, i)) (List.rev defns)) @ argOffset env n in
    Let n :: (compDefns defns newEnv @ compC body newEnv)
  | _ -> failwith "Unsupported expression in abstract syntax tree"

and compSc (sc:scdefn) (env:env) : instruction list =
  let (name, args, body) = sc in
  Printf.fprintf stdout "SC: %s\n" name;
  let transformedBody = preprocessAppToLet body in
  let compBody = compC transformedBody ((List.mapi (fun i a -> (a, i)) args) @ (argOffset env (List.length args))) in
  if (List.length args) > 0 then Take :: compBody else compBody

and compProg (prog:program) =
  let env = makeScEnv prog in
  List.map (fun sc -> let (name, args, body) = sc in (name, List.length args, (compSc sc env))) prog

and findFreeVars (expr:expr) (freeVars:string list) =
  match expr with
  | Var s -> s :: freeVars
  | App (e1, e2) -> findFreeVars e1 freeVars @ findFreeVars e2 freeVars
  | Let (defns, body) | Letrec (defns, body) ->
      let letVars = List.flatten (List.map (fun (_, e) -> findFreeVars e freeVars) defns) @
                    findFreeVars body freeVars in
      removeVars (List.map (fun (v,_) -> v) defns) letVars
  | Case (cond, alts) -> freeVars
  | Num _ | Pack _ | Sel _ -> freeVars

and removeVars (vars:string list) (freeVars:string list) =
  match freeVars with
  | [] -> []
  | x::xs -> if (List.exists (fun y -> x = y) freeVars) then
               removeVars xs freeVars
             else
               x :: (removeVars xs freeVars)

and makeScEnv (prog:program) =
  List.mapi (fun i (name, _, _) -> (name, i)) prog
;;
