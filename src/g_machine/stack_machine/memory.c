/**
From the code accompanying Programming Language Concepts by Peter Sestoft

The methods markPhase, sweepPhase and mark have been added by Mathias and Mads
**/

#include <stdlib.h>
#include <stdio.h>
#include "memory.h"
#include "stack_machine.h"
#include "debug.h"

word* heap;
word* afterHeap;
word* freelist;
int indirection_node_orphans = 0;

word make_header(unsigned int tag, unsigned int length, unsigned int color) {
  return (tag << 24) | (length << 2) | color;
}

void init_heap(int heap_size) {
  heap = (word*)malloc(sizeof(word)*heap_size);
  afterHeap = &heap[heap_size];
  // Initially, entire heap is one block on the freelist:
  heap[0] = make_header(0, heap_size-1, Blue);
  heap[1] = (word)0;
  freelist = &heap[0];
}

// Call this after a GC to get heap statistics:
void heapStatistics() {
  printf("Orphans from conversion to indirection node: %d\n", indirection_node_orphans);
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
      printf("Next block at heap[%d]\n", nextBlock-heap);
      exit(-1);
    }
    heapPtr = nextBlock;
  }
  word* freePtr = freelist;
  while (freePtr != 0) {
    free++;
    int length = GetLength(freePtr[0]);
    if (length == 0)
    {
      printf("Something on the freelist has length 0!!\n");
    }
    if (freePtr < heap || afterHeap < freePtr+length+1) {
      printf("HEAP ERROR: freelist item %d (at heap[%d], length %d) is outside heap\n",
	     free, freePtr-heap, length);
      exit(-1);
    }
    freeSize += length;
    largestFree = length > largestFree ? length : largestFree;
    if (GetColor(freePtr[0])!=Blue)
      printf("Non-blue block at heap[%d] on freelist\n", (int)freePtr);
    freePtr = (word*)freePtr[1];
  }
  printf("Heap: %d blocks (%d words); of which %d free (%d words, largest %d words); %d orphans\n",
	 blocks, blocksSize, free, freeSize, largestFree, orphans);
}

void mark(word* block)
{
  // This block has already been reached
  if (GetColor(block[0]) == Black)
  {
    return;
  }
  // If the block is not black it must be white

  // If the block is of length zero it is an orphan
  if (GetLength(block[0]) == 0)
  {
    block[0] = Paint(block[0], Blue);
    return;
  }

  block[0] = Paint(block[0], Black);

  switch(GetTag(block[0]))
  {
    case APP_NODE:
    {
      mark((word*) block[1]);
      mark((word*) block[2]);
      break;
    }
    case INTEGER_NODE:
    {
      break;
    }
    case GLOBAL_NODE:
    {
      break;
    }
    case IND_NODE:
    {
      mark((word*) block[1]);
      break;
    }
    case NULL_NODE:
    {
      break;
    }
    case PACK_NODE:
    {
      break;
    }
    case CONSTR_NODE:
    {
      int arity = block[2];
      for (int i = 0; i < arity; i++)
      {
        if ((word*) block[3+i] != NULL)
        {
          mark((word*) block[3+i]);
        }
        else
        {
          printf("Element of Constructor Node is NULL\n");
        }
      }
      break;
    }
  }
}

void markPhase(word** s, int sp) {
  //printf("marking ...\n");
  for (int i = 0; i <= sp; i++)
  {
    word* p = s[i];
    mark(p);
  }
}

void sweepPhase() {
  //printf("sweeping ...\n");
  word* heapPtr = heap;
  while (heapPtr < afterHeap) {
    switch (GetColor(heapPtr[0]))
    {
      case White:
      {
        // Join together adjacent dead blocks
        word nextHeader = heapPtr[GetLength(heapPtr[0])+1];
        int length = GetLength(heapPtr[0]);
        while (GetColor(nextHeader) == White)
        {
          length = length + GetLength(nextHeader) + 1;
          nextHeader = heapPtr[length+1];
        }
        heapPtr[0] = make_header(99, length, Blue);
        // Add block to the front of the freelist
        heapPtr[1] = (word) freelist;
        freelist = heapPtr;
        break;
      }
      case Black:
      {
        heapPtr[0] = Paint(heapPtr[0], White);
        break;
      }
    }
    heapPtr = heapPtr + GetLength(heapPtr[0]) + 1;
  }
}

void collect(word** s, int sp) {
  markPhase(s, sp);
  //heapStatistics();
  sweepPhase();
  //heapStatistics();
}

word* allocate_block(unsigned int tag, unsigned int length, word** s, int sp) {
  if (length == 0) printf("Allocating block of length 0\n");
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
  printf("Trying to allocate block for tag %s with length %d\n", tag_to_name(tag), length);
  heapStatistics();
  exit(1);
}

void convert_block_to_indirection_node(word* block)
{
  int length = GetLength(block[0]);
  int rest = length - 2;
  if (rest < 0)
  {
    printf("Trying to convert block of length %d to indirection node\n", length);
    exit(1);
  }
  block[0] = make_header(IND_NODE, 2, White);
  if (rest == 0)
  {
    return;
  }
  else if (rest == 1)
  {
    // Color orphan blue so it is not put on freelist
    block[2] = make_header(99, 0, Blue);
    indirection_node_orphans++;
  }
  else
  {
    // Color the rest white so it will be recovered in next sweep
    block[2] = make_header(99, rest-1, White);
  }
}