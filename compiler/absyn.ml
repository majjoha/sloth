type program = scdefn list

and scdefn = string * string list * expr

and expr =
  | Var of string
  | Num of int
  | Constr of int * int
  | App of expr * expr
  | Let of bool * (string * expr) list * expr
  | Case of expr * alt list

and alt = int * string list * expr
