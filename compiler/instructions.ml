type instructions =
  | Pushglobal of string
  | Push of int
  | Pushint of int
  | Mkap
  | Unwind
  | Slide of int
