{
  open Parser
  open Lexing

  exception SyntaxError of string
}

rule main = parse
  | ' ' | '\t' | '\n'   { main lexbuf }
  | ['0'-'9']+          { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ','                 { COMMA }
  | ['a'-'z' '_']+ as s { IDENT s }
  | '+'                 { PLUS  }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '='                 { EQ    } 
  | '('                 { LPAR  }
  | ')'                 { RPAR  }
  | _                   { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof                 { EOF }
