#ifndef MEMORY_H
#define MEMORY_H

#define White 0
#define Grey  1
#define Black 2
#define Blue  3

#define APP_NODE     0
#define INTEGER_NODE 1
#define GLOBAL_NODE  2
#define IND_NODE 3
#define NULL_NODE 4
#define PACK_NODE 5

#define GetTag(h)    ((h)>>24)
#define GetLength(h) (((h)>>2)&0x3fffff)
#define Color(h) ((h)&3)
#define Paint(h,c) (((h)&(~3))|(c))

typedef unsigned int word;

word make_header(unsigned int tag, unsigned int length, unsigned int color);
word* allocate_block(unsigned int tag, unsigned int length, int sp, word** s);
void init_heap(int heap_size);
void mark(word* block);
void collect(word** s, int sp);

#endif
