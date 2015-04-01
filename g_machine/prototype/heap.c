#include "heap.h"
#include "list.h"
#include "ti_node.h"
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

void heap_update(heap_t* heap, address_t address, ti_node_t* new_node) {
  node_t* node = heap->associations->first;
  while ((node = node->next) != NULL) {
    association_object_t* association_node = (association_object_t*) node->elm;
    if (address == association_node->address) {
      association_node->object = new_node;
    }
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
