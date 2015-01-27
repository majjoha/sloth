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
