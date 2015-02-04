#include "ti_node.h"

#ifndef _STACK_H
#define _STACK_H

#define DUMP_BOTTOM  -2
#define STACK_BOTTOM -1

typedef struct Stack {
  address_t* contents;
  int top;
  int max_size;
  int offset;
} ti_stack_t;

void stack_init(ti_stack_t* stack);

address_t stack_pop(ti_stack_t* stack);

void stack_push(ti_stack_t* stack, address_t address);

void push_new_stack(ti_stack_t* stack);

#endif
