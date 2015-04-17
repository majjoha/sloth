#include <stdio.h>
#include <stdlib.h>
#include "memory.h"

word* freelist;
word* heap;
word* afterHeap;

word make_header(unsigned int tag, unsigned int length, unsigned int color) { 
  return (tag << 24) | (length << 2) | color;
}

word* allocate_block(unsigned int tag, unsigned int length, int sp, word** s) {
  int attempt = 1;
  do {
    word* free = freelist;
    word** prev = &freelist;
    while (free != 0) {
      int rest = GetLength(free[0]) - length;
      if (rest >= 0)  {
        if (rest == 0) // Exact fit with free block
          *prev = (word*)free[1];
        else if (rest == 1) { // Create orphan (unusable) block
          *prev = (word*)free[1];
          free[length+1] = make_header(0, rest-1, Blue);
        } else { // Make previous free block point to rest of this block
          *prev = &free[length+1];
          free[length+1] = make_header(0, rest-1, Blue);
          free[length+2] = free[1];
        }
        free[0] = make_header(tag, length, White);
        return free;
      }
      prev = (word**)&free[1];
      free = (word*)free[1];
    }
    // No free space, do a garbage collection and try again
    if (attempt==1)
      collect(s, sp);
  } while (attempt++ == 1);
  printf("Out of memory\n");
  exit(1);
}

void heapStatistics() {
  int blocks = 0, free = 0, orphans = 0, 
    blocksSize = 0, freeSize = 0, largestFree = 0;
  word* heapPtr = heap;
  while (heapPtr < afterHeap) {
    if (GetLength(heapPtr[0]) > 0) {
      blocks++;
      blocksSize += GetLength(heapPtr[0]);
    } else 
      orphans++;
    word* nextBlock = heapPtr + GetLength(heapPtr[0]) + 1;
    if (nextBlock > afterHeap) {
      printf("HEAP ERROR: block at heap[%d] extends beyond heap\n", 
	     heapPtr-heap);
      exit(-1);
    }
    heapPtr = nextBlock;
  }
  word* freePtr = freelist;
  while (freePtr != 0) {
    free++; 
    int length = GetLength(freePtr[0]);
    if (freePtr < heap || afterHeap < freePtr+length+1) {
      printf("HEAP ERROR: freelist item %d (at heap[%d], length %d) is outside heap\n", 
	     free, freePtr-heap, length);
      exit(-1);
    }
    freeSize += length;
    largestFree = length > largestFree ? length : largestFree;
    if (Color(freePtr[0])!=Blue)
      printf("Non-blue block at heap[%d] on freelist\n", (int)freePtr);
    freePtr = (word*)freePtr[1];
  }
  printf("Heap: %d blocks (%d words); of which %d free (%d words, largest %d words); %d orphans\n", 
	 blocks, blocksSize, free, freeSize, largestFree, orphans);
}

void init_heap(int heap_size) {
  heap = (word*)malloc(sizeof(word)*heap_size);
  afterHeap = &(heap[heap_size]);
  // Initially, entire heap is one block on the freelist.
  heap[0] = make_header(0, heap_size-1, Blue);
  heap[1] = (word)0;
  freelist = &heap[0];
}

void markPhase(word** s, int sp) {
  printf("marking ...\n");

  for (int counter = sp; counter >= 0; counter--) {
    mark((word*) s[counter]);
  }
}

void mark(word* block) {
  if (Color(block[0]) == White) {
    block[0] = Paint(block[0], Black);
    
    switch (GetTag(block[0])) {
      case APP_NODE: {
        mark((word*) block[1]);
        mark((word*) block[2]);
        break;
      }
      case INTEGER_NODE: {
        break;
      }
      case GLOBAL_NODE: {
        break;
      }
      case IND_NODE: {
        mark((word*) block[1]);
        break;
      }
      case NULL_NODE: {
        break;
      }
      case PACK_NODE: {
        // TODO: Figure out  how to mark this properly.
        break;
      }
    }
  }
}

void sweepPhase() {
  printf("sweeping ...\n"); 
  
  word* heapPtr = heap;
  word* freePtr = freelist;

  while(heapPtr < afterHeap) {
    switch(Color(heapPtr[0])) {
      case Black: 
        heapPtr[0] = Paint(heapPtr[0], White);
        break;
      case White:
        heapPtr[0] = Paint(heapPtr[0], Blue);
        
        word headerNext = heapPtr[GetLength(heapPtr[0])+1];
        if(Color(headerNext) == White){
          heapPtr[0] = make_header(0, 1 + GetLength(heapPtr[0]) + GetLength(headerNext), Blue);
        }
        
        heapPtr[1] = (word) freelist;
        freelist = heapPtr;
        break; 
    }
    heapPtr = heapPtr + GetLength(heapPtr[0]) + 1;
  }
}

void collect(word** s, int sp) {
  markPhase(s, sp);
  heapStatistics();
  sweepPhase();
  heapStatistics();
}
