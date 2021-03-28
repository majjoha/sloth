{
  open Parser
  open Lexing

  exception SyntaxError of string

  let keyword s =
    match s with
    | "in"    -> IN
    | "let"   -> LET
    | "letrec" -> LETREC
    | "case" -> CASE
    | "of" -> OF
    | "pack" -> PACK
    | "end"  -> END
    | _       -> IDENT s
}


rule token = parse
  | ' ' | '\t' | '\n'   { token lexbuf }
  | ['0'-'9']+          { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ','                 { COMMA  }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as s { keyword(s) }
  | '+'                 { PLUS   }
  | '-'                 { MINUS  }
  | '*'                 { TIMES  }
  | '/'                 { DIV    }
  | '<'                 { LT     }
  | '>'                 { GT     }
  | "<="                { LE     }
  | ">="                { GE     }
  | "!="                { NEQ    }
  | "&&"                { AND    }
  | "||"                { OR     }
  | "->"                { RARROW }
  | '='                 { EQ     }
  | '('                 { LPAR   }
  | ')'                 { RPAR   }
  | '#'                 { HEAD   }
  | '_'                 { TAIL   }
  | ':'                 { CONS   }
  | ';'                 { SEMI   }
  | _                   { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof                 { EOF    }
