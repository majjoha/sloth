%{
  open Absyn
%}

%token <int> INT
%token <string> IDENT
%token IN LET LETREC CASE OF PACK
%token COMMA
%token PLUS MINUS TIMES DIV
%token LT GT LE GE NEQ EQ
%token OR AND
%token RARROW
%token LPAR
%token RPAR
%token EOF

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
  | s1 = scdefn; s2 = scdefns               { s1 :: s2 }
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
  | u = unop; a = aexpr;                     { App(u, a)               }
  | e1 = expr; PLUS; e2 = expr               { App(App(Var "PLUS", e1), e2) }
  | e1 = expr; MINUS; e2 = expr              { App(App(Var "MINUS", e1), e2) }
  | e1 = expr; b = binop; e2 = expr          { App(App(b, e1), e2)     }
  | LET; d = defns; IN; e = expr             { Let(false, d, e)        }
  | LETREC; d = defns; IN; e = expr          { Let(true, d, e)         }
  | CASE; e = expr; OF; a = alts             { Case(e, a)              }
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

unop:
  | MINUS;                                      { Var "NEG"  }

binop:
  | a = arithop                                 { a }
  | r = relop                                   { r }
  | b = boolop                                  { b }
;

arithop:
  | PLUS                                  { Var "PLUS"  }
  | MINUS                                 { Var "MINUS" }
  | TIMES                                 { Var "TIMES" }
  | DIV                                   { Var "DIV"   }
;

relop:
  | LT                                    { Var "LT"  }
  | LE                                    { Var "LE"  }
  | EQ                                    { Var "EQ"  }
  | NEQ                                   { Var "NEQ" }
  | GT                                    { Var "GT"  }
  | GE                                    { Var "GE"  }
;

boolop:
  | AND                                   { Var "AND" }
  | OR                                    { Var "OR"  }
;
