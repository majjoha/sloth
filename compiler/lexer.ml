# 1 "lexer.mll"
 
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

# 20 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\230\255\231\255\232\255\233\255\234\255\235\255\236\255\
    \237\255\238\255\001\000\001\000\002\000\003\000\030\000\247\255\
    \248\255\249\255\030\000\251\255\078\000\253\255\153\000\255\255\
    \239\255\244\255\243\255\242\255\241\255\240\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\024\000\024\000\024\000\010\000\009\000\255\255\
    \255\255\255\255\005\000\255\255\003\000\255\255\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\023\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \023\000\012\000\000\000\006\000\000\000\015\000\011\000\028\000\
    \008\000\007\000\017\000\019\000\021\000\018\000\000\000\016\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\004\000\003\000\014\000\009\000\013\000\027\000\
    \026\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\025\000\024\000\000\000\000\000\005\000\
    \000\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\000\000\010\000\029\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\011\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
    \013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\014\000\018\000\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\010\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\255\255\255\255\255\255\255\255\255\255\255\255\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 21 "lexer.mll"
                        ( token lexbuf )
# 164 "lexer.ml"

  | 1 ->
# 22 "lexer.mll"
                        ( INT (int_of_string (Lexing.lexeme lexbuf)) )
# 169 "lexer.ml"

  | 2 ->
# 23 "lexer.mll"
                        ( COMMA  )
# 174 "lexer.ml"

  | 3 ->
let
# 24 "lexer.mll"
                                                s
# 180 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 24 "lexer.mll"
                                                  ( keyword(s) )
# 184 "lexer.ml"

  | 4 ->
# 25 "lexer.mll"
                        ( PLUS   )
# 189 "lexer.ml"

  | 5 ->
# 26 "lexer.mll"
                        ( MINUS  )
# 194 "lexer.ml"

  | 6 ->
# 27 "lexer.mll"
                        ( TIMES  )
# 199 "lexer.ml"

  | 7 ->
# 28 "lexer.mll"
                        ( DIV    )
# 204 "lexer.ml"

  | 8 ->
# 29 "lexer.mll"
                        ( MOD    )
# 209 "lexer.ml"

  | 9 ->
# 30 "lexer.mll"
                        ( LT     )
# 214 "lexer.ml"

  | 10 ->
# 31 "lexer.mll"
                        ( GT     )
# 219 "lexer.ml"

  | 11 ->
# 32 "lexer.mll"
                        ( LE     )
# 224 "lexer.ml"

  | 12 ->
# 33 "lexer.mll"
                        ( GE     )
# 229 "lexer.ml"

  | 13 ->
# 34 "lexer.mll"
                        ( NEQ    )
# 234 "lexer.ml"

  | 14 ->
# 35 "lexer.mll"
                        ( AND    )
# 239 "lexer.ml"

  | 15 ->
# 36 "lexer.mll"
                        ( OR     )
# 244 "lexer.ml"

  | 16 ->
# 37 "lexer.mll"
                        ( RARROW )
# 249 "lexer.ml"

  | 17 ->
# 38 "lexer.mll"
                        ( EQ     )
# 254 "lexer.ml"

  | 18 ->
# 39 "lexer.mll"
                        ( LPAR   )
# 259 "lexer.ml"

  | 19 ->
# 40 "lexer.mll"
                        ( RPAR   )
# 264 "lexer.ml"

  | 20 ->
# 41 "lexer.mll"
                        ( HEAD   )
# 269 "lexer.ml"

  | 21 ->
# 42 "lexer.mll"
                        ( TAIL   )
# 274 "lexer.ml"

  | 22 ->
# 43 "lexer.mll"
                        ( CONS   )
# 279 "lexer.ml"

  | 23 ->
# 44 "lexer.mll"
                        ( SEMI   )
# 284 "lexer.ml"

  | 24 ->
# 45 "lexer.mll"
                        ( raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) )
# 289 "lexer.ml"

  | 25 ->
# 46 "lexer.mll"
                        ( EOF    )
# 294 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;
