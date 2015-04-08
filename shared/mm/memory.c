#include <stdlib.h>
#include "memory.h"

word make_header(unsigned int tag, unsigned int length, unsigned int color) { 
  return (tag << 24) | (length << 2) | color;
}

word* allocate_word(unsigned int tag, unsigned int length, word** lastFreeHeapNode) {
  **lastFreeHeapNode = make_header(tag, length, Blue);
  word* heapNode = *lastFreeHeapNode;
  *lastFreeHeapNode = *lastFreeHeapNode + length + 1;
  return heapNode;
}

void init_heap(word** heap, word** afterHeap, word** lastFreeHeapNode, int heap_size) {
  *heap = (word*)malloc(sizeof(word)*heap_size);
  *afterHeap = &(*heap[heap_size]);
  *lastFreeHeapNode = &(*heap[0]);
}
