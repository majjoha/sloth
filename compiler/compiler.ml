open Instructions
open Absyn

type env = (string * int) list;;

let failwith msg = raise (Failure msg);;

let rec inEnv (env:env) (var:string) =
  match env with
  | [] -> false
  | (x1,x2)::xs -> if x1 = var then true else inEnv xs var
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
  | _ -> failwith "Unimplemented expression type."
;;

let rec compR (expr:expr) (env:env) =
  compC expr env @ [Slide (List.length env + 1); Unwind]
;;

let rec compSc (sc:scdefn) =
  match sc with
  | (name, args, body) ->
      let i = ref (-1) in
      (name, List.length args,
        compR body (List.map (fun a -> i := !i+1; (a,!i)) args))
;;

let rec compProg (prog:program) =
  match prog with
  | [] -> []
  | x::xs -> compSc x :: compProg xs
;;
