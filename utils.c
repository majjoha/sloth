#include "ti.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

ti_node_t* heap_lookup(heap_t* heap, address_t address) {
  node_t* node = heap->associations->first;
  while ((node = node->next) != NULL) {
    association_object_t* association_node = (association_object_t*) node->elm;
    if (address == association_node->address) {
      return association_node->object;
    }
  }
  
  printf("Did not find anything in heap_lookup\n");
  return NULL;
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

int is_data_node(ti_node_t* node) {
  if (node->type == NUM) {
    return TRUE;
  }

  return FALSE;
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
    ti_node_t* ti_node = heap_lookup(heap, stack->contents[i]);
    if (ti_node->type == APP) {
      args[i] = ti_node->data.app_data.address2;
    } else {
      exit(1);
    }
  }
  return args;
}

void create_num_node(int n, ti_node_t* node)
{
  node->type = NUM;
  node->data.num_data = n;
} 

address_t binding_lookup(char* name, list_t* bindings)
{
  node_t* node = bindings->first;
  while ((node = node->next) != NULL)
  {
    binding_t* binding = (binding_t*) node->elm;
    if (strcmp(binding->name, name) == 0)
    {
      return binding->address;
    }
  }
  return -1;
}

address_t a_lookup(char* name, list_t* bindings, globals_t* globals)
{
  address_t address = binding_lookup(name, bindings);
  if (address == -1)
  {
    address = binding_lookup(name, globals);
  }
  return address;
}

address_t instantiate(expr_t* body, heap_t* heap, globals_t* globals, list_t* bindings)
{
  switch (body->tag)
  {
    case E_NUM:
      {
      int n = body->data.e_num;
      ti_node_t* node = malloc(sizeof(ti_node_t));
      create_num_node(n, node);
      return heap_alloc(heap, node);
    }
    case VAR:
      {
        char* name = body->data.e_variable;
        address_t address = a_lookup(name, bindings, globals);
        if (address == -1)
        {
          exit(1);
        }
        return address;
      }
    case APP:
    {
      e_application_t* app = body->data.e_application;
      address_t a1 = instantiate(app->expr1, heap, globals, bindings);
      address_t a2 = instantiate(app->expr2, heap, globals, bindings);
      ti_node_t* node = malloc (sizeof(ti_node_t));
      node->type = APP;
      app_data_t* app_data = malloc(sizeof(app_data_t));
      app_data->address1 = a1;
      app_data->address2 = a2;
      node->data.app_data = *app_data;
      return heap_alloc(heap, node);
    }
    case LET:
      return -1;
  }
}

void num_step(state_t* state, int n) {
  printf("Number applied as a function!\n");
}

void sc_step(state_t* state, sc_data_t sc_data) {
  address_t *args = get_args(state->stack, state->heap);

  list_t *bindings = list_new();

  for (int i = 0; i < sc_data.arg_names_count; i++)
  {
    binding_t *binding = (binding_t*) malloc(sizeof(binding_t));
    binding->name = sc_data.arg_names[i];
    binding->address = args[i];
    list_add_anything(bindings, binding);
  }

  address_t result_address = instantiate(sc_data.body, state->heap, state->globals, bindings);

  for (int i = 0; i < sc_data.arg_names_count+1; i++)
  {
    stack_pop(state->stack);
  }
  stack_push(state->stack, result_address);
}

void app_step(state_t* state, app_data_t app_data) {
  stack_push(state->stack, app_data.address1);
}

void dispatch(state_t* state) {
  address_t address = stack_pop(state->stack);
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

int ti_final(ti_stack_t* stack, heap_t* heap) {
  if (stack->top == 0) {
    printf("Empty stack!\n");
    exit(1);
  }

  if (stack->top > 1) {
    return FALSE;
  }

  ti_node_t* node = heap_lookup(heap, stack->contents[0]);
  if (is_data_node(node))
  {
    return TRUE;
  }
  else 
  {
    printf("Last node on stack is not data node\n");
    return FALSE;
  }
}

void eval(state_t *state)
{
  while(!ti_final(state->stack, state->heap))
  {
    dispatch(state);
  }
}

heap_t* heap_init() {
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

binding_t* allocate_sc(heap_t* heap, sc_defn_t* sc) {
  ti_node_t* sc_node = malloc(sizeof(ti_node_t));
  sc_node->type = SC;
  sc_node->data.sc_data = *sc;
  int sc_address = heap_alloc(heap, sc_node);
  binding_t* sc_global = malloc(sizeof(binding_t));
  sc_global->name = sc->sc_name;
  sc_global->address = sc_address;

  return sc_global;
}

globals_t* build_initial_heap(heap_t* heap, sc_defn_t** scs, int sc_count)
{
    globals_t *globals = list_new();
    for (int i = 0; i < sc_count; i++)
    {
        binding_t* global = allocate_sc(heap, (*scs)++);
        list_add_anything(globals, global);
    }
    return globals;
}


int main(int argc, const char *argv[])
{
  sc_defn_t *scs[1]; 
  sc_defn_t *sc = malloc(sizeof(sc_defn_t));
  sc->sc_name = "main";
  char *a[0];
  // a[0] = "blah";
  // a[1] = "hmm";
  sc->arg_names = a;
  sc->arg_names_count = 0;
  expr_t *e = malloc(sizeof(expr_t));
  e->data.e_num = 1;
  e->tag = E_NUM;
  sc->body = e;
  scs[0] = sc;
  heap_t *heap = heap_init();
  globals_t* globals = build_initial_heap(heap, scs, 1);
  // node_t* global_node = list_remove(globals);
  // binding_t* global = global_node->elm;
  // printf("sc name: %s\n", global->name);
  // printf("sc address: %d\n", global->address);
  // printf("heap size: %d\n", heap->count);
  // node_t* node;
  // while ((node = list_remove(heap->unused_addresses)) != NULL)
  // {
  //   int *address = (int *)node->elm;
  //   printf("unused address: %d\n", *address);
  // }
  ti_stack_t *stack = malloc(sizeof(ti_stack_t));
  stack_init(stack);
  binding_t* binding = (binding_t*) globals->first->next->elm;
  stack_push(stack, binding->address);
  printf("Binding address: %d\n", binding->address);
  association_object_t* assoc_obj = heap->associations->first->next->elm;
  printf("Address in heap: %d\n", assoc_obj->address);
  state_t *state = malloc(sizeof(state_t));
  state->heap = heap;
  state->globals = globals;
  state->stack = stack;
  printf("Før eval\n");
  eval(state);
  address_t result = stack_pop(state->stack);
  ti_node_t *result_node = heap_lookup(state->heap, result);
  printf("Result: %d\n", result_node->data.num_data);
  //printf("heap ")
  return 0;
}





