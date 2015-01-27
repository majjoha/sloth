typedef char* e_variable_t; 

typedef int e_num_t;

typedef struct Expr expr_t;

typedef struct Application {
  expr_t* expr1;
  expr_t* expr2;
} e_application_t;

typedef struct Let {
  struct { char* name; expr_t* expr; } *defs;
  expr_t* body; 
} e_let_t;

struct Expr {
  union {
    e_variable_t e_variable;
    e_num_t e_num;
    e_application_t e_application;
    e_let_t e_let;
  } data;
  enum { VAR, NUM, APP, LET } tag;
}; 
