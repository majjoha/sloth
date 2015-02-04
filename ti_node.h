#ifndef _TINODE_H
#define _TINODE_H

#include "absyn.h"

typedef int address_t;

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

#endif