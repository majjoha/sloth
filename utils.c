#include "ti.h"
#include <stdlib.h>

int main(int argc, const char *argv[])
{
  return 0;
}

heap_t* heap_initial() {
  heap_t* heap = (heap_t *) malloc(sizeof(heap_t));
  heap->count = 0;
  heap->unused_addresses = list_new();
  for(int i = 1; i <= 100; i++) {
    list_add(heap->unused_addresses, node_new_anything(&i));
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
