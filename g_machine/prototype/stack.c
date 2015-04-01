#include <stdlib.h>
#include "ti_node.h"
#include "stack.h"
#include <stdio.h>

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

  printf("Trying to pop from empty stack.\n");
  return -1;
}

address_t stack_peek(ti_stack_t* stack) {
  if (stack->top != 0) {
    return stack->contents[stack->top-1];
  }

  printf("Trying to peek value on an empty stack.\n");
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

void print_stack(ti_stack_t* stack) {
  printf("Stack top: %d\n", stack->top);
  printf("Stack offset: %d\n", stack->offset);
  printf("Stack contents:\n");
  for (int i = 0; i < stack->top; i++) {
    printf("%d: %d\n", i, stack->contents[i]);
  }
}
