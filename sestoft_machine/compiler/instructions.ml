type instruction = 
  | Take
  | Enter of int
  | Push of int
  | Sep
  | Let of int
  | Enterglobal of string
  | Pushglobal of string