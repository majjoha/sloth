type expr =
  | CstI of int
  | Prim of string * expr * expr
