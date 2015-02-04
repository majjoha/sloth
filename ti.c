#include "ti.h"
#include "stack.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int is_data_node(ti_node_t* node) {
  if (node->type == NUM) {
    return TRUE;
  }

  return FALSE;
}

address_t* get_args(ti_stack_t* stack, heap_t* heap) {
  address_t* args = malloc(sizeof(address_t)*(stack->top-stack->offset));
  for (int i = stack->top - 1; i >= stack->offset; i--) {
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
    case E_PRIM:
    {
      e_prim_t prim = body->data.e_prim;
      ti_node_t* node = malloc(sizeof(ti_node_t));
      node->type = PRIM;
      node->data.prim_data = prim.op;
      return heap_alloc(heap, node);
    }
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

  for (int i = 0; i < sc_data.arg_names_count; i++)
  {
    stack_pop(state->stack);
  }
  stack_push(state->stack, result_address);
}

void app_step(state_t* state, app_data_t app_data) {
  stack_push(state->stack, app_data.address1);
}

void prim_step(state_t* state, int prim_data) {
  switch(prim_data) {
    case NEG:
    {
      address_t address = state->stack->contents[state->stack->top-1];
      ti_node_t* node = heap_lookup(state->heap, address);
      if (node->type == APP) {
        address_t address = node->data.app_data.address2;
        ti_node_t* node2 = heap_lookup(state->heap, address);

        if (node2->type == NUM) {
          heap_update(state->heap, address, node2);
        } else {
          push_new_stack(state->stack);
          stack_push(state->stack, node->data.app_data.address2);
        }
      } else {
        printf("Primitive is not applied to APP node.");
        exit(1);
      }
      break;
    }
  }
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
    case PRIM:
      prim_step(state, node->data.prim_data);
      break;
  }
}

int ti_final(ti_stack_t* stack, heap_t* heap) {
  if (stack->top == stack->offset) {
    printf("Empty stack!\n");
    exit(1);
  }

  if (stack->top > stack->offset) {
    return FALSE;
  }

  ti_node_t* node = heap_lookup(heap, stack->contents[stack->offset]);
  if (is_data_node(node))
  {
    if (stack->contents[stack->offset-2] == DUMP_BOTTOM) {
      return TRUE;
    } else {
      stack_pop(stack);
      stack_pop(stack);
      address_t offset = stack_pop(stack);
      stack->offset = offset;
      return FALSE;
    }
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


  expr_t *e1 = malloc(sizeof(expr_t));
  e1->data.e_prim.op = NEG;
  e1->tag = E_PRIM;

  expr_t *e2 = malloc(sizeof(expr_t));
  e2->data.e_num = 1;
  e2->tag = E_NUM;

  expr_t *app = malloc(sizeof(expr_t));
  app->data.e_application = malloc(sizeof(e_application_t));
  app->data.e_application->expr1 = e1;
  app->data.e_application->expr2 = e2;
  app->tag = APP;

  sc->body = app;
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
