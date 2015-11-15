#ifndef MEMORY_H
#define MEMORY_H

#define White 0
#define Grey  1
#define Black 2
#define Blue  3

#define GetTag(h)    ((h)>>24)
#define GetLength(h) (((h)>>2)&0x3fffff)
#define GetColor(h)  ((h)&3)
#define Paint(h, c)  (((h)&(~3))|(c))

typedef unsigned int word;

word* allocate_block(unsigned int tag, unsigned int length, word** s, int sp);
void init_heap(int heap_size);
void convert_block_to_indirection_node(word* block);

#endif