#include "absyn.h"
#include "list.h"
#include "heap.h"
#include "stack.h"
#include "ti_node.h"

#define TRUE  1
#define FALSE 0

#ifndef _TI_H
#define _TI_H

typedef sc_data_t sc_defn_t;

typedef struct Binding {
  char* name;
  address_t address;
} binding_t;

typedef list_t globals_t;

typedef struct State {
  ti_stack_t* stack;
  heap_t* heap;
  globals_t* globals;
} state_t;

#endif
