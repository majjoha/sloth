type instruction =
  | Pushglobal of string
  | Push of int
  | Pushint of int
  | Mkap
  | Unwind
  | Slide of int
  | Jump of string
  | Update of int
  | Pop of int
  | Alloc of int
  | Eval
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | Eq
  | Ne
  | Le
  | Lt
  | Ge
  | Gt
  | Jfalse of string
  | Label of string
  | Pack of int * int
  | Split of int
  | Casejump of string list