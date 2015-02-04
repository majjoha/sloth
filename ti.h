#include "absyn.h"
#include "list.h"

#define TRUE  1
#define FALSE 0

typedef int address_t;

typedef struct Stack {
  address_t* contents;
  int top;
  int max_size;
} ti_stack_t;

typedef struct AppData {
  address_t address1;
  address_t address2;
} app_data_t;

typedef struct SCData {
  char* sc_name;
  char** arg_names;
  int arg_names_count;
  expr_t* body;
} sc_data_t;

typedef struct TiNode {
  enum { NUM, SC, APP } type;
  union {
    app_data_t app_data;
    sc_data_t sc_data;
    int num_data;
  } data;
} ti_node_t;

typedef sc_data_t sc_defn_t;

typedef struct Binding {
  char* name;
  address_t address;
} binding_t;

typedef list_t globals_t;

typedef struct AssociationObject {
  address_t address;
  ti_node_t* object;
} association_object_t;

typedef struct Heap {
  int count;
  list_t* unused_addresses;
  list_t* associations; 
} heap_t;

typedef struct State {
  ti_stack_t* stack;
  heap_t* heap;
  globals_t* globals;
} state_t;
