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

let binopToScName (op:string) =
  match op with
  | "+" -> "add"
  | "-" -> "sub"
  | "*" -> "mul"
  | "/" -> "div"
  | "=" -> "eq"
  | "neq" -> "neq"
  | "<" -> "lt"
  | ">" -> "gt"
  | "ge" -> "ge"
  | "le" -> "le"
  | _    -> failwith ("Unknown binary operator: "^op)
;;

let binopToIns (op:string) =
  match op with
  | "+" -> Add  
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "=" -> Eq
  | "neq" -> Ne
  | "<" -> Lt
  | ">" -> Gt
  | "ge" -> Ge
  | "le" -> Le
  | _    -> failwith ("Unknown binary operator: "^op)
;;

let rec compLet (env:env) (defns:(string * expr) list) (body:expr) (compBody:expr -> env -> instruction list) =
  let compDefns = List.flatten (List.mapi (fun i (s, e) -> compC e (argOffset env i)) defns) in
  let newEnv = (List.mapi (fun i (s, e) -> (s, i)) (List.rev defns)) @ argOffset env (List.length defns) in
  compDefns @ (compBody body newEnv) @ [Slide (List.length defns)]

and compLetrec (env:env) (defns:(string * expr) list) (body:expr) (compBody:expr -> env -> instruction list) =
  let defnsLen = List.length defns in
  let newEnv = (List.mapi (fun i (s, e) -> (s, i)) (List.rev defns)) @ argOffset env (List.length defns) in
  let compDefns = List.flatten (List.mapi (fun i (s, e) -> compC e newEnv @ [Update (defnsLen - (i+1))]) defns) in
  [Alloc (List.length defns)] @ compDefns @ (compBody body newEnv) @ [Slide (List.length defns)]

and compA (alt:alt) (label:string) (endLabel:string) (env:env) =
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
  | Let (defns, body) -> compLet env defns body compC
  | Letrec (defns, body) -> compLetrec env defns body compC
  | Case (e, alts) ->
      let compExpr = compC e env in
      let ls = List.map (fun (t, _, _) -> (t, newLabel())) alts in
      let endLabel = newLabel() in
      compExpr @ [Eval; Casejump ls] @ (compAlts alts ls endLabel env)
  | Pack (t, a) ->
      [Pack (t, a)]
  | Binop(op, e1, e2) -> 
      compC e2 env 
      @ compC e1 (argOffset env 1) 
      @ [Pushglobal (binopToScName op); Mkap; Mkap]
  | Unop(op, e) -> 
      if op = "-" 
      then compC e env @ [Pushglobal "neg"; Mkap]
      else failwith ("Unknown unary operator: "^op) 
  | If(e1, e2, e3) ->
      compC e3 env
      @ compC e2 (argOffset env 1)
      @ compC e1 (argOffset env 2)
      @ [Pushglobal "if"; Mkap; Mkap; Mkap]
  | _ -> failwith "Unimplemented expression type."
;;

let rec compE (expr:expr) (env:env) =
  match expr with
  | Let (defns, body) -> compLet env defns body compE
  | Letrec (defns, body) -> compLetrec env defns body compE
  | Binop(op, e1, e2) -> 
    compE e2 env @ compE e1 (argOffset env 1) @ [binopToIns op]
  | Unop(op, e) ->
    (match op with
    | "-" -> compE e env @ [Neg]
    | _ -> failwith ("Unknown unary operator: "^op))
  | If(e1,e2,e3) ->
    compE e1 env @  [Jfalse "L1"] @ compE e2 env @ [Jfalse "L2"; Label "L1"] @ compE e3 env @ [Label "L2"; Update 0]
  | _ -> compC expr env

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
