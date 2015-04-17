open Absyn

let generateVarName =
  Printf.sprintf "%f" (Sys.time ()) |> Digest.string |> Digest.to_hex;;
;;

let rec preprocessAppToLet (body:expr) = 
  match body with
  | Var s -> Var s
  | Num i -> Num i
  | Pack (i1, i2) -> Pack (i1, i2)
  | Sel (i1, i2) -> Sel (i1, i2)
  | App (e1, e2) ->
      (match e2 with
      | Var s -> App (preprocessAppToLet e1, e2)
      | _ -> let varName = generateVarName in
             Let ([(varName, preprocessAppToLet e2)],
                  App(preprocessAppToLet e1, Var varName)))
  | Let (defns, body) ->
      let newDefns = List.map (fun (s, e) -> (s, preprocessAppToLet e)) defns in
      let newBody = preprocessAppToLet body in
      Let (newDefns, newBody)
  | Letrec (defns, body) ->
      let newDefns = List.map (fun (s, e) -> (s, preprocessAppToLet e)) defns in
      let newBody = preprocessAppToLet body in
      Letrec (newDefns, newBody)
  | Case (e, alts) -> 
      let newAlts = List.map (fun (i, s, e) -> (i, s, preprocessAppToLet e)) alts in
      let newExpr = preprocessAppToLet e in
      Case (newExpr, newAlts)
;;
