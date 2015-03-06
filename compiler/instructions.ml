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
