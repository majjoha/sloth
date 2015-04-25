type instruction = 
  | Take
  | Enter of int
  | Push of int
  | Sepcase
  | Let of int
  | Enterglobal of string
  | Pushglobal of string
  | Freevars of int list
  | Case of int
  | Pack of int * int
  | CstI of int
  | Add
  | Sub
  | Mul
  | Div
  | Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Neg
  | Seplet
