#include <stdlib.h>
#include "ti_node.h"
#include "stack.h"

void stack_init(ti_stack_t* stack) {
  stack->max_size = 10000;
  stack->contents = malloc(sizeof(address_t)*stack->max_size);
  stack->top = 0;
  stack->offset = 2;
  stack_push(stack, DUMP_BOTTOM);
  stack_push(stack, STACK_BOTTOM);
}

address_t stack_pop(ti_stack_t* stack) {
  if (stack->top != 0) {
    int* stack_top = &stack->top;
    (*stack_top)--;
    int top = stack->contents[*stack_top];
    return top;
  }

  return -1;
}

void stack_push(ti_stack_t* stack, address_t address) {
  if (stack->top < stack->max_size) {
    stack->contents[stack->top++] = address;
  }
}

void push_new_stack(ti_stack_t* stack) {
  stack_push(stack, stack->offset);
  stack_push(stack, STACK_BOTTOM);
  stack->offset = stack->top;
}
