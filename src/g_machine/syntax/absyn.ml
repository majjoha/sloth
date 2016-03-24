type program = scdefn list

and operator = string

and scdefn = string * string list * expr

and expr =
  | Var of string
  | Num of int
  | Pack of int * int
  | Sel of int * int
  | App of expr * expr
  | Let of (string * expr) list * expr
  | Letrec of (string * expr) list * expr
  | Case of expr * alt list
  | Binop of operator * expr * expr
  | Unop of operator * expr
  | If of expr * expr * expr

and alt = int * string list * expr
