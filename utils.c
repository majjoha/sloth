#include "ti.h"
#include <stdlib.h>
#include <stdio.h>

ti_node_t* heap_lookup(heap_t* heap, address_t address) {
  node_t* node = heap->associations->first;
  while ((node = node->next) != NULL) {
    association_object_t* association_node = (association_object_t*) node->elm;
    if (address == association_node->address) {
      return association_node->object;
    }
  }
  
  return NULL;
}

int is_data_node(ti_node_t* node) {
  if (node->type == NUM) {
    return TRUE;
  }

  return FALSE;
}

state_t* eval(state_t* states) {

}

void step(state_t* state) {

  
}

void stack_init(ti_stack_t* stack) {
  stack->max_size = 10000;
  stack->contents = malloc(sizeof(address_t)*stack->max_size);
  stack->top = 0;
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

address_t* get_args(ti_stack_t* stack, heap_t* heap) {
  address_t* args = malloc(sizeof(address_t)*(stack->top-1));
  for (int i = stack->top - 2; i >= 0; i--) {
    ti_node_t* node = heap_lookup(heap, stack->contents[i]);
    if (node->type == APP) {
      args[i] = node->data.app_data.address2;
    } else {
      exit(1);
    }
  }
}

void num_step(state_t* state, int n) {
  printf("Number applied as a function!\n");
}

void sc_step(state_t* state, sc_data_t sc_data) {
}

void app_step(state_t* state, app_data_t app_data) {
}

void dispatch(ti_stack_t* stack, state_t* state) {
  address_t address = stack_pop(stack);
  ti_node_t* node = heap_lookup(state->heap, address);
  switch (node->type) {
    case NUM:
      num_step(state, node->data.num_data);
      break;
    case SC:
      sc_step(state, node->data.sc_data);
      break;
    case APP:
      app_step(state, node->data.app_data);
      break;
  }
}

int ti_final(ti_stack_t* stack, address_t address, heap_t* heap) {
  if (stack->top == 0) {
    printf("Empty stack!\n");
  }

  if (stack->top > 1) {
    return FALSE;
  }

  return is_data_node(heap_lookup(heap, address));
}

heap_t* heap_initial() {
  heap_t* heap = (heap_t *) malloc(sizeof(heap_t));
  heap->count = 0;
  heap->unused_addresses = list_new();
  for(int i = 1; i <= 100; i++) {
    int* j = malloc(sizeof(int));
    *j = i;
    list_add(heap->unused_addresses, node_new_anything(j));
  }
  heap->associations = list_new();
  return heap;
}

int heap_alloc(heap_t* heap, ti_node_t* ti_node) {
  heap->count++;

  node_t* next_address_node = list_remove(heap->unused_addresses);
  int next_address = *((int *) next_address_node->elm);

  association_object_t* temp_assoc_obj = malloc(sizeof(association_object_t));
  temp_assoc_obj->address = next_address;
  temp_assoc_obj->object = ti_node;

  list_add(heap->associations, node_new_anything(temp_assoc_obj));

  return next_address;
}

global_t* allocate_sc(heap_t* heap, sc_defn_t* sc) {
  ti_node_t* sc_node = malloc(sizeof(ti_node_t));
  sc_node->type = SC;
  sc_node->data.sc_data = *sc;
  int sc_address = heap_alloc(heap, sc_node);
  global_t* sc_global = malloc(sizeof(global_t));
  sc_global->name = sc->sc_name;
  sc_global->address = sc_address;

  return sc_global;
}

globals_t* build_initial_heap(heap_t* heap, sc_defn_t** scs, int sc_count)
{
    globals_t *globals = list_new();
    for (int i = 0; i < sc_count; i++)
    {
        global_t* global = allocate_sc(heap, *scs++);
        list_add_anything(globals, global);
    }
    return globals;
}


int main(int argc, const char *argv[])
{
  sc_defn_t *scs[1]; 
  sc_defn_t *sc = malloc(sizeof(sc_defn_t));
  sc->sc_name = "main";
  char *a[2];
  a[0] = "blah";
  a[1] = "hmm";
  sc->arg_names = a;
  expr_t *e = malloc(sizeof(expr_t));
  e->data.e_num = 1;
  e->tag = NUM;
  sc->expr = e;
  scs[0] = sc;
  heap_t *heap = heap_initial();
  globals_t* globals = build_initial_heap(heap, scs, 1);
  node_t* global_node = list_remove(globals);
  global_t* global = global_node->elm;
  printf("sc name: %s\n", global->name);
  printf("sc address: %d\n", global->address);
  printf("heap size: %d\n", heap->count);
  node_t* node;
  while ((node = list_remove(heap->unused_addresses)) != NULL)
  {
    int *address = (int *)node->elm;
    printf("unused address: %d\n", *address);
  }
  //printf("heap ")
  return 0;
}





