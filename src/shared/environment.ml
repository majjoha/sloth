open Printf

type env = (string * int) list;;

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