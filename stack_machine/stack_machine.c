#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"

#define STACK_SIZE 1000
#define HEAP_SIZE 1000
#define LAST_INSTRUCTION -1

#define PUSHGLOBAL 0
#define PUSH 1
#define PUSHINT 2
#define MKAP 3
#define UNWIND 4
#define SLIDE 5
#define JUMP 6

#define APP_NODE 0
#define INTEGER_NODE 1
#define GLOBAL_NODE 2

#define White 0
#define Grey  1
#define Black 2
#define Blue  3

#define GetTag(h) (h>>24)
#define GIndex(i) sp-(i)

typedef unsigned int word;

word* heap;
word* afterHeap;
word* lastFreeHeapNode;

word mkheader(unsigned int tag, unsigned int length, unsigned int color) { 
  return (tag << 24) | (length << 2) | color;
}

void init_heap() {
  heap = (word*)malloc(sizeof(word)*HEAP_SIZE);
  afterHeap = &heap[HEAP_SIZE];
  lastFreeHeapNode = &heap[0];
}

word* allocate(unsigned int tag, unsigned int length) {
  *lastFreeHeapNode = mkheader(tag, length, Blue);
  word* heapNode = lastFreeHeapNode;
  lastFreeHeapNode = lastFreeHeapNode + length + 1;
  return heapNode;
}

void print_instructions(int* instructions) {
  for (int i = 0; instructions[i] != LAST_INSTRUCTION; i++) {
    switch (instructions[i]) {
      case PUSHGLOBAL: { printf("PUSHGLOBAL %d; ", instructions[++i]); break; }
      case PUSH: { printf("PUSH %d; ", instructions[++i]); break; }
      case PUSHINT: { printf("PUSHINT %d; ", instructions[++i]); break; }
      case MKAP: { printf("MKAP; "); break; }
      case UNWIND: { printf("UNWIND; "); break; }
      case SLIDE: { printf("SLIDE %d; ", instructions[++i]); break; }
      case JUMP: { printf("JUMP %d; ", instructions[++i]); break; }
      default: printf("<unknown> ");
    }
  }
  printf("\n");
}

void print_stack(int sp, int* stack) {
  for (int i = 0; i != sp+1; i++) {
    printf("%d\n", GetTag(*(word*)stack[i]));
  }
}

int execute_instructions(int* program, int* stack) {
  int sp = -1;
  int pc = 0;

  for (;;) {
    switch (program[pc++]) {
      case PUSHGLOBAL: {
        word* global_node = allocate(GLOBAL_NODE, 1);
        global_node[1] = program[pc++];
        stack[++sp] = (int) global_node;
        break;
      }
      case PUSH: {
        int next = program[pc++];
        printf("%d\n", GIndex(next+1));
        word* app_node = (word*) stack[GIndex(next+1)];
        stack[sp+1] = app_node[2];
        sp++;
        break;
      }
      case PUSHINT: {
        word* integer_node = allocate(INTEGER_NODE, 1);
        integer_node[1] = program[pc++];
        stack[++sp] = (int) integer_node;
        break;
      }
      case MKAP: {
        word left = stack[sp];
        word right = stack[sp-1];
        word* app_node = allocate(APP_NODE, 2);
        app_node[1] = left;
        app_node[2] = right;
        stack[sp-1] = (int) app_node;
        sp--;
        break;
      }
      case UNWIND: {
        switch (GetTag(*(word*)stack[sp])) {
          case INTEGER_NODE: {
            word* integer_node = (word*) stack[sp];
            return integer_node[1];
          }  
          case APP_NODE: {
            word* app_node = (word*) stack[sp];
            stack[++sp] = app_node[1];
            pc--;
            break;
          }
          case GLOBAL_NODE: {
            word* global_node = (word*) stack[sp];
            pc = (int) global_node[1];
            break;
          }
          default:
            printf("Unwind on %d\n", GetTag(*(word*)stack[sp]));
            exit(EXIT_FAILURE);
        }
        break;
      }
      case SLIDE: {
        word* node = (word*) stack[sp];
        int n = program[pc++];
        if (n > sp) {
          sp = 0;
        } else {
          sp = GIndex(n);
        }
        stack[sp] = (int) node;
        break;
      }
      case JUMP:
        pc = program[pc+1];
        break;
      default:
        printf("Illegal instruction %d at address %d\n", program[pc-1], pc-1);
        exit(EXIT_FAILURE);
    }
  }

  return 1;
}

int execute(char* filename) {
  int* program = read_file(filename);
  int* stack = (int*)malloc(sizeof(int) * STACK_SIZE);
  init_heap();

  print_instructions(program);

  return execute_instructions(program, stack);
}

int main(int argc, char* argv[]) {
  if (argc == 2) {
    return execute(argv[1]);
  } else {
    printf("You need to pass a file to the stack machine.\n");
    printf("Usage: stack_machine <program>\n");
    exit(EXIT_SUCCESS);
  }

  return 0;
}
