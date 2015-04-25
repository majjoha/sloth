#include "mm/memory.h"

int address_to_heap_index(word* node, word* heap) {
  return node - heap;
}

int unbox_integer(word* word) {
  return word[1];
}
