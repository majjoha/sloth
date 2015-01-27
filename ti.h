#include "absyn.h"
#include "list.h"

typedef int address_t;
typedef address_t* ti_stack_t;

typedef struct AppData {
  address_t address1;
  address_t address2;
} app_data_t;

typedef struct SCData {
  char* sc_name;
  char** arg_names;
  expr_t* expr;
} sc_data_t;

typedef struct TiNode {
  enum type { NUM, SC, APP };
  union {
    app_data_t app_data;
    sc_data_t sc_data;
    int num_data;
  } data;
} ti_node_t;

typedef struct AssociationGlobal {
  char* name;
  address_t address;
} association_global_t;

typedef association_global_t* globals_t;

typedef struct AssociationObject {
  int address;
  ti_node_t object;
} association_object_t;

typedef struct Heap {
  int count;
  list_t* unused_addresses;
  list_t *associations; 
} heap_t;
