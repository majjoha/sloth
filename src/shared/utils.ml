open Absyn

let failwith msg = raise (Failure msg);;

let rec exprToStr expr =
  match expr with
  | Var _ -> "Var"
  | Num _ -> "Num"
  | Pack _ -> "Pack"
  | Sel _ -> "Sel"
  | App _ -> "App"
  | Let _ -> "Let"
  | Letrec _ -> "Letrec"
  | Case _ -> "Case"
;;
