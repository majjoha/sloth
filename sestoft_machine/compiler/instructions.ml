type instruction = 
  | Take
  | Enter of int
  | Push of int
  | Sep
  | Let of int
  | Enterglobal of string
  | Pushglobal of string
  | Freevars of int list
  | Case of int
  | Pack of int * int
