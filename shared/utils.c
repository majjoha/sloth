#include "mm/memory.h"

int address_to_heap_index(word* node, word* heap) {
  return node - heap;
}