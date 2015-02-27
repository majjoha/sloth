%{
  open Absyn
%}

%token <int> INT
%token <string> IDENT
%token IN LET LETREC CASE OF PACK
%token COMMA SEMI
%token PLUS MINUS TIMES DIV MOD
%token LT GT LE GE NEQ EQ
%token OR AND
%token RARROW
%token LPAR
%token RPAR
%token EOF
%token CONS HEAD TAIL

%right HEAD TAIL
%right CONS
%left EQ NEQ
%nonassoc GT LT GE LE
%left PLUS MINUS
%left TIMES DIV
%left OR
%left AND

%start <Absyn.program> prog

%%

prog:
  | scs = scdefns; EOF                     { scs }
;

scdefns:
  | s1 = scdefn; SEMI; s2 = scdefns         { s1 :: s2 }
  | s = scdefn                              { [s] }
;

scdefn:
  | i1 = IDENT; i2 = idents; EQ; e = expr   { (i1, i2, e) }
;

idents:
  | s = IDENT; i = idents                    { s :: i }
  | s = IDENT                                { [s]    }
  |                                          { []     }
;

expr:
  | e = expr; a = aexpr                      { App(e, a)               }
  | e1 = expr; PLUS; e2 = expr               { App(App(Var "PLUS", e1), e2) }
  | e1 = expr; MINUS; e2 = expr              { App(App(Var "MINUS", e1), e2) }
  | e1 = expr; TIMES; e2 = expr              { App(App(Var "TIMES", e1), e2) }
  | e1 = expr; DIV; e2 = expr                { App(App(Var "DIV", e1), e2) }
  | e1 = expr; MOD; e2 = expr                { App(App(Var "MOD", e1), e2) }
  | e1 = expr; LT; e2 = expr                 { App(App(Var "LT", e1), e2) }
  | e1 = expr; GT; e2 = expr                 { App(App(Var "GT", e1), e2) }
  | e1 = expr; EQ; e2 = expr                 { App(App(Var "EQ", e1), e2) }
  | e1 = expr; NEQ; e2 = expr                { App(App(Var "NEQ", e1), e2) }
  | e1 = expr; LE; e2 = expr                 { App(App(Var "LE", e1), e2) }
  | e1 = expr; GE; e2 = expr                 { App(App(Var "GE", e1), e2) }
  | e1 = expr; AND; e2 = expr                { App(App(Var "AND", e1), e2) }
  | e1 = expr; OR; e2 = expr                 { App(App(Var "OR", e1), e2) }
  | MINUS; e = expr                          { App(Var "NEG", e)          }
  | LET; d = defns; IN; e = expr             { Let(d, e)                  }
  | LETREC; d = defns; IN; e = expr          { Letrec(d, e)            }
  | CASE; e = expr; OF; a = alts             { Case(e, a)              }
  | HEAD; e = expr                           { App(Sel(2, 1), e)      }
  | TAIL; e = expr                           { App(Sel(2, 2), e)      }
  | e1 = expr; CONS; e2 = expr               { App(App(Constr(2, 2), e1), e2) }
  | a = aexpr                                { a                       }
;

aexpr:
  | s = IDENT                                   { Var s                }
  | i = INT                                     { Num i                }
  | PACK; LPAR; i1 = INT; COMMA; i2 = INT; RPAR { Constr(i1, i2)       }
  | LPAR; e = expr; RPAR                        { e }
;

defns:
  | d = defn; ds = defns                        { d :: ds    }
  | d = defn                                    { [d]        }
;

defn:
  | i = IDENT; EQ; e = expr;                    { (i, e)     }
;

alts:
  | a = alt; als = alts                         { a :: als   }
  | a = alt;                                    { [a]        }
;

alt:
  | i = INT; is = idents; RARROW; e = expr      { (i, is, e) }
;
