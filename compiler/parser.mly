%{
  open Absyn
%}

%token <int> INT
%token <string> IDENT
%token COMMA
%token PLUS
%token MINUS
%token TIMES
%token EQ
%token LPAR
%token RPAR
%token EOF

%start <Absyn.expr option> main

%%

main:
  | EOF         { None   }
  | e = expr    { Some e }
;

expr:
  | LPAR; PLUS; e1 = expr; e2 = expr; RPAR  { Prim("+", e1, e2) }
  | LPAR; MINUS; e1 = expr; e2 = expr; RPAR { Prim("-", e1, e2) }
  | c = const                               { c                 }
;

const:
  | i = INT                                 { CstI i            }
;
