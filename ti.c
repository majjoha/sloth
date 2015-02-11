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
  for (int i = stack->top - 2; i >= stack->offset; i--) {
    ti_node_t* ti_node = heap_lookup(heap, stack->contents[i]);
    if (ti_node->type == APP) {
      args[i] = ti_node->data.app_data.address2;
    } else {
      print_stack(stack);
      printf("Exiting due to argument to supercombinator is not application node.\n");
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

    printf("Name in binding_lookup: %s\n", name);
    printf("Address in binding_looup: %lu\n", (long) name);
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
        char* name = *body->data.e_variable;
        printf("%s\n", name);
        printf("%lu\n", (long) name);
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

  for (int i = 0; i < sc_data.arg_names_count+1; i++)
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
      address_t address = state->stack->contents[state->stack->top-2];
      ti_node_t* node = heap_lookup(state->heap, address);
      if (node->type == APP) {
        address_t address2 = node->data.app_data.address2;
        ti_node_t* node2 = heap_lookup(state->heap, address2);

        if (node2->type == NUM) {
          heap_update(state->heap, address, node2);
        } else {
          push_new_stack(state->stack);
          stack_push(state->stack, node->data.app_data.address2);
        }
      } else if (node->type == NUM) {
        node->data.num_data = -node->data.num_data;
        stack_pop(state->stack);
      } else {
        printf("Primitive is not applied to APP node.\n");
        exit(1);
      }
      break;
    }
  }
}

void dispatch(state_t* state) {
  address_t address = stack_peek(state->stack);
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

  if (stack->top > stack->offset+1) {
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
  /* printf("In allocate_sc: %s\n", *sc_node->data.sc_data.body->data.e_variable); */
  /* printf("In allocate_sc: %s\n", *sc->body->data.e_variable); */
  int sc_address = heap_alloc(heap, sc_node);
  binding_t* sc_global = malloc(sizeof(binding_t));
  sc_global->name = sc->sc_name;
  sc_global->address = sc_address;

  return sc_global;
}

globals_t* build_initial_heap(heap_t* heap, sc_defn_t** scs, int sc_count)
{
    globals_t *globals = list_new();
    printf("%s\n", *scs[0]->body->data.e_variable);
    for (int i = 0; i < sc_count; i++)
    {
        sc_defn_t* sc_definition = malloc(sizeof(sc_defn_t));
        sc_definition = scs[i];
        binding_t* global = allocate_sc(heap, scs[i]);
        list_add_anything(globals, global);
    }
    return globals;
}

int main(int argc, const char *argv[])
{
  // We create an array of supercombinator definitions.
  sc_defn_t *scs[2]; 

  // We create the main supercombinator with 0 arguments.
  sc_defn_t *sc = malloc(sizeof(sc_defn_t));
  sc->sc_name = "main";
  char *a[0];
  sc->arg_names = a;
  sc->arg_names_count = 0;

  // We create a negation expression.
  expr_t *e1 = malloc(sizeof(expr_t));
  e1->data.e_prim.op = NEG;
  e1->tag = E_PRIM;

  // We create a variable expression "x".
  expr_t *e2 = malloc(sizeof(expr_t));
  char* x = "x";
  e2->data.e_variable = &x;
  e2->tag = VAR;

  // We create an application expr with negation on the left side, and the
  // variable on the right side.
  expr_t *neg_expr = malloc(sizeof(expr_t));
  neg_expr->data.e_application = malloc(sizeof(e_application_t));
  neg_expr->data.e_application->expr1 = e1;
  neg_expr->data.e_application->expr2 = e2;
  neg_expr->tag = APP;

  // The negation supercombinator which takes one argument x.
  sc_defn_t *neg_sc = malloc(sizeof(sc_defn_t));
  neg_sc->sc_name = "negation";
  char *arg_names[1];
  arg_names[0] = "x";
  neg_sc->arg_names = arg_names;
  neg_sc->arg_names_count = 1;
  neg_sc->body = neg_expr;

  /* expr_t *neg = malloc(sizeof(expr_t)); */
  /* neg->data.e_prim.op = NEG; */
  /* neg->tag = E_PRIM; */

  /* expr_t* negneg = malloc(sizeof(expr_t)); */
  /* negneg->data.e_application = malloc(sizeof(e_application_t)); */
  /* negneg->data.e_application->expr1 = neg; */
  /* negneg->data.e_application->expr2 = app; */
  /* negneg->tag = APP; */

  expr_t* the_number_42 = malloc(sizeof(expr_t));
  the_number_42->tag = E_NUM;
  the_number_42->data.e_num = 42;

  expr_t* neg_variable = malloc(sizeof(expr_t));
  neg_variable->tag = VAR;
  char* neg_name = malloc(sizeof(char)*10);
  neg_name = "negation";
  neg_variable->data.e_variable = &neg_name;
  printf("%s\n", *neg_variable->data.e_variable);
  printf("%lu\n", (long) *neg_variable->data.e_variable);

  expr_t* main_app = malloc(sizeof(expr_t));
  main_app->data.e_application = malloc(sizeof(e_application_t));
  main_app->data.e_application->expr1 = neg_variable;
  printf("%s\n", *main_app->data.e_application->expr1->data.e_variable);
  main_app->data.e_application->expr2 = the_number_42;
  main_app->tag = APP;
  sc->body = main_app;
  scs[0] = sc;
  scs[1] = neg_sc;
  heap_t *heap = heap_init();
  globals_t* globals = build_initial_heap(heap, scs, 2);

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
  return 0;
}
