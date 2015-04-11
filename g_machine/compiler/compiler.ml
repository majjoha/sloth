open Instructions
open Absyn
open Printf
open Environment
open Utils

type compiledSc = (string * int * instruction list);;

let nextlab = ref 2;;

let newLabel () = (nextlab := 1 + !nextlab; "L" ^ (string_of_int !nextlab));;

let compPrim =
  [
    ("add", 2, [Push 1; Eval; Push 1; Eval; Add; Update 2; Pop 2; Unwind]);
    ("sub", 2, [Push 1; Eval; Push 1; Eval; Sub; Update 2; Pop 2; Unwind]);
    ("mul", 2, [Push 1; Eval; Push 1; Eval; Mul; Update 2; Pop 2; Unwind]);
    ("div", 2, [Push 1; Eval; Push 1; Eval; Div; Update 2; Pop 2; Unwind]);
    ("neg", 1, [Push 0; Eval; Neg; Update 1; Pop 1; Unwind]);
    ("eq",  2, [Push 1; Eval; Push 1; Eval; Eq; Update 2; Pop 2; Unwind]);
    ("neq", 2, [Push 1; Eval; Push 1; Eval; Ne; Update 2; Pop 2; Unwind]);
    ("lt",  2, [Push 1; Eval; Push 1; Eval; Lt; Update 2; Pop 2; Unwind]);
    ("le",  2, [Push 1; Eval; Push 1; Eval; Le; Update 2; Pop 2; Unwind]);
    ("gt",  2, [Push 1; Eval; Push 1; Eval; Gt; Update 2; Pop 2; Unwind]);
    ("ge",  2, [Push 1; Eval; Push 1; Eval; Ge; Update 2; Pop 2; Unwind]);
    ("if",  3, [Push 0; Eval; Jfalse "L1"; Push 1; Jump "L2"; Label "L1";
               Push 2; Label "L2"; Eval; Update 3; Pop 3; Unwind])
  ]
;;

let rec compA (alt:alt) (label:string) (endLabel:string) (env:env) =
  let (tag, vars, body) = alt in
  let n = List.length vars in
  let newEnv = ((List.mapi (fun i var -> (var, i))) vars) @ (argOffset env n) in
  [Label label; Split n] @ compC body newEnv @ [Slide n; Jump endLabel]

and compAlts (alts:alt list) (labels:(int * string) list) (endLabel:string) (env:env) =
  let zipped = List.combine alts labels in
  let compAlts = List.map (fun (a, (t, l)) -> compA a l endLabel env) zipped in
  (List.flatten compAlts) @ [Label endLabel]

and compC (expr:expr) (env:env) =
  match expr with
  | Var s -> if inEnv env s then [Push (lookup env s)] else [Pushglobal s]
  | Num n -> [Pushint n]
  | App (e1, e2) -> compC e2 env @ compC e1 (argOffset env 1) @ [Mkap]
  | Let (defns, body) -> 
      let compDefns = List.flatten (List.mapi (fun i (s, e) -> compC e (argOffset env i)) defns) in
      let newEnv = (List.mapi (fun i (s, e) -> (s, i)) (List.rev defns)) @ argOffset env (List.length defns) in
      compDefns @ (compC body newEnv) @ [Slide (List.length defns)]
  | Letrec (defns, body) ->
      let defnsLen = List.length defns in
      let newEnv = (List.mapi (fun i (s, e) -> (s, i)) (List.rev defns)) @ argOffset env (List.length defns) in
      let compDefns = List.flatten (List.mapi (fun i (s, e) -> compC e newEnv @ [Update (defnsLen - (i+1))]) defns) in
      [Alloc (List.length defns)] @ compDefns @ (compC body newEnv) @ [Slide (List.length defns)]
  | Case (e, alts) ->
      let compExpr = compC e env in
      let ls = List.map (fun (t, _, _) -> (t, newLabel())) alts in
      let endLabel = newLabel() in
      compExpr @ [Eval; Casejump ls] @ (compAlts alts ls endLabel env)
  | Pack (t, a) ->
      [Pack (t, a)]
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

let compProg (prog:program) =
  let rec compP (prog:program) =
    match prog with
    | [] -> []
    | x::xs -> compSc x :: compP xs in
  compPrim @ compP prog
;;
