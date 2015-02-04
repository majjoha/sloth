#include "list.h"
#include "ti_node.h"

#ifndef _HEAP_H
#define _HEAP_H

typedef struct AssociationObject {
  address_t address;
  ti_node_t* object;
} association_object_t;

typedef struct Heap {
  int count;
  list_t* unused_addresses;
  list_t* associations; 
} heap_t;

ti_node_t* heap_lookup(heap_t* heap, address_t address);

int heap_alloc(heap_t* heap, ti_node_t* ti_node);

heap_t* heap_init();

#endif