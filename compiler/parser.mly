%{
  open Absyn
%}

%token <int> INT
%token <string> IDENT
%token IN LET LETREC CASE OF PACK END
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
%left TIMES DIV MOD
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
  | e1 = expr; PLUS; e2 = expr               { App(App(Var "add", e1), e2) }
  | e1 = expr; MINUS; e2 = expr              { App(App(Var "sub", e1), e2) }
  | e1 = expr; TIMES; e2 = expr              { App(App(Var "mul", e1), e2) }
  | e1 = expr; DIV; e2 = expr                { App(App(Var "div", e1), e2) }
  | e1 = expr; MOD; e2 = expr                { App(App(Var "mod", e1), e2) }
  | e1 = expr; LT; e2 = expr                 { App(App(Var "lt", e1), e2) }
  | e1 = expr; GT; e2 = expr                 { App(App(Var "gt", e1), e2) }
  | e1 = expr; EQ; e2 = expr                 { App(App(Var "eq", e1), e2) }
  | e1 = expr; NEQ; e2 = expr                { App(App(Var "neq", e1), e2) }
  | e1 = expr; LE; e2 = expr                 { App(App(Var "le", e1), e2) }
  | e1 = expr; GE; e2 = expr                 { App(App(Var "ge", e1), e2) }
  | e1 = expr; AND; e2 = expr                { App(App(Var "and", e1), e2) }
  | e1 = expr; OR; e2 = expr                 { App(App(Var "or", e1), e2) }
  | MINUS; e = aexpr                         { App(Var "neg", e)          }
  | HEAD; e = expr                           { App(Sel(2, 1), e)      }
  | TAIL; e = expr                           { App(Sel(2, 2), e)      }
  | e1 = expr; CONS; e2 = expr               { App(App(Pack(2, 2), e1), e2) }
  | a = aexpr                                { a                       }
  | ap = appexpr                             { ap }
  | LET; d = defns; IN; e = expr             { Let(d, e)               }
  | LETREC; d = defns; IN; e = expr          { Letrec(d, e)            }
  | CASE; e = expr; OF; a = alts; END        { Case(e, a)              }
;

aexpr:
  | s = IDENT                                   { Var s                }
  | i = INT                                     { Num i                }
  | LPAR; e = expr; RPAR                        { e }
  | PACK; LPAR; i1 = INT; COMMA; i2 = INT; RPAR { Pack(i1, i2)       }
;

appexpr:
  | a1 = aexpr; a2 = aexpr                      { App(a1, a2) }
  | e = appexpr; a = aexpr                      { App(e, a)               }
;

defns:
  | d = defn; SEMI; ds = defns                  { d :: ds    }
  | d = defn                                    { [d]        }
;

defn:
  | i = IDENT; EQ; e = expr;                    { (i, e)     }
;

alts:
  | a = alt; SEMI; als = alts                   { a :: als   }
  | a = alt;                                    { [a]        }
;

alt:
  | i = INT; is = idents; RARROW; e = expr      { (i, is, e) }
;
