#ifndef MEMORY_H
#define MEMORY_H

#define White 0
#define Grey  1
#define Black 2
#define Blue  3

typedef unsigned int word;

word make_header(unsigned int tag, unsigned int length, unsigned int color);
word* allocate_word(unsigned int tag, unsigned int length, word** lastFreeHeapNode);
void init_heap(word** heap, word** afterHeap, word** lastFreeHeapNode, int heap_size);

#endif
