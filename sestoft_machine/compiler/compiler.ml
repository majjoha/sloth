open Instructions
open Absyn
open Printf
open Environment
open Utils
open Preprocessor

type compiledSc = (string * int * instruction list);;

let compPrim =
  [
    ("add", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Add; Sepcase 1; Sepcase 0]);
    ("sub", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Sub; Sepcase 1; Sepcase 0]);
    ("mul", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Mul; Sepcase 1; Sepcase 0]);
    ("div", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Div; Sepcase 1; Sepcase 0]);
    ("lt", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Lt; Sepcase 1; Sepcase 0]);
    ("gt", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Gt; Sepcase 1; Sepcase 0]);
    ("le", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Le; Sepcase 1; Sepcase 0]);
    ("ge", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Ge; Sepcase 1; Sepcase 0]);
    ("eq", 2, [Take; Take; Case (0, 1); Enter 1; Sepcase 0; Case (1, 1); Enter 1; Sepcase 1; Eq; Sepcase 1; Sepcase 0]);
    ("neg", 1, [Take; Case (0, 1); Enter 0; Sepcase 0; Neg; Sepcase 0]);
  ]
;;

let rec compFvs (vars:string list) (env:env) =
  Freevars (List.map (fun s -> lookup env s) vars)

and compDefns (defns:(string * expr) list) (env:env) (caseDepth:int) (letDepth:int) =
  List.fold_right (fun (s, e) acc -> (compFvs (findFreeVars e []) env) ::
                                     (compC e env caseDepth (letDepth+1)) @
                                     (Seplet letDepth :: acc)) defns []

and compAlt alt env caseDepth letDepth =
  let (tag, vars, body) = alt in
  let length = List.length vars in
  let newEnv = List.mapi (fun i var -> (var, i)) (List.rev vars) @ argOffset env length in
  compC body newEnv (caseDepth+1) letDepth @ [Sepcase caseDepth]

and compAlts alts env caseDepth letDepth =
  List.flatten (List.map (fun alt -> compAlt alt env caseDepth letDepth) (List.sort (fun (t1,_,_) (t2,_,_) -> compare t1 t2) alts))

and compC (expr:expr) (env:env) (caseDepth:int) (letDepth:int) : instruction list =
  match expr with
  | App(e1, e2) -> 
    (match e2 with
    | Var s -> Push (lookup env s) :: compC e1 env caseDepth letDepth
    | _     -> failwith "Function applied to non-variable expression")
  | Var s -> Printf.fprintf stdout "%s\n" s; [Enter (lookup env s)]
  | Num i -> [CstI i]
  | Letrec (defns, body) | Let (defns, body) -> 
    let n = List.length defns in
    let newEnv = (List.mapi (fun i (s, e) -> (s, i)) (List.rev defns)) @ argOffset env n in
    Let (letDepth, n) :: (compDefns defns newEnv caseDepth letDepth @ compC body newEnv caseDepth letDepth)
  | Case(e, alts) -> 
    let n = List.length alts in
    Case (caseDepth, n) :: (compC e env (caseDepth+1) letDepth @ [Sepcase caseDepth] @ compAlts alts env caseDepth letDepth)
  | Pack(tag, arity) ->
    (repeat Take arity) @ [Instructions.Pack (tag, arity)]
  | _ -> failwith ("Unsupported expression in abstract syntax tree: " ^ exprToStr expr)

and compSc (sc:scdefn) (env:env) : instruction list =
  let (name, args, body) = sc in
  Printf.fprintf stdout "SC: %s\n" name;
  let transformedBody = preprocessAppToLet body in
  let compBody = compC transformedBody ((List.mapi (fun i a -> (a, i)) (List.rev args)) @ (argOffset env (List.length args))) 0 0 in
  let args_count = List.length args in
  (repeat Take args_count) @ compBody

and compProg (prog:program) =
  let env = makeScEnv prog compPrim in
  printEnv env;
  compPrim @
  (List.map (fun sc -> let (name, args, body) = sc in (name, List.length args, (compSc sc env))) prog)

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

and removeVars (letBoundVars:string list) (freeVars:string list) =
  match freeVars with
  | [] -> []
  | x::xs -> if (List.exists (fun y -> x = y) letBoundVars) then
               removeVars letBoundVars xs
             else
               x :: (removeVars letBoundVars xs)

and makeScEnv (prog:program) (compPrim:compiledSc list) =
  List.mapi (fun i name -> (name, i))
    ((List.map (fun (name,_,_) -> name) compPrim) @
     (List.map (fun (name,_,_) -> name) prog))

and repeat thing n = 
  if n = 0 
  then [] 
  else thing :: repeat thing (n-1)
;;
