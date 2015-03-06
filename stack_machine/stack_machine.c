#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "stack_machine.h"

word* heap;
word* afterHeap;
word* lastFreeHeapNode;
int verbose = 0;

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

int execute_instructions(int* program, word** stack) {
  int sp = -1;
  int pc = 0;

  for (;;) {
    if (verbose) {
      int vpc = pc;
      print_stack(sp, stack);
      print_instruction(program, &vpc);
      printf("\n");
    }

    switch (program[pc++]) {
      case PUSHGLOBAL: {
        word* global_node = allocate(GLOBAL_NODE, 1);
        global_node[1] = program[pc++];
        stack[++sp] = global_node;
        break;
      }
      case PUSH: {
        int next = program[pc++];
        word* app_node = stack[GtoAIndex(next+1)];
        stack[sp+1] = (word*) app_node[2];
        sp++;
        break;
      }
      case PUSHINT: {
        word* integer_node = allocate(INTEGER_NODE, 1);
        integer_node[1] = program[pc++];
        stack[++sp] = integer_node;
        break;
      }
      case MKAP: {
        word* left = stack[sp];
        word* right = stack[sp-1];
        word* app_node = allocate(APP_NODE, 2);
        app_node[1] = (word) left;
        app_node[2] = (word) right;
        stack[sp-1] = app_node;
        sp--;
        break;
      }
      case UNWIND: {
        switch (GetTag(*stack[sp])) {
          case INTEGER_NODE: {
            word* integer_node = stack[sp];
            return integer_node[1];
          }  
          case APP_NODE: {
            word* app_node = stack[sp];
            stack[++sp] = (word*) app_node[1];
            pc--;
            break;
          }
          case GLOBAL_NODE: {
            word* global_node = stack[sp];
            pc = (int) global_node[1];
            break;
          }
          case IND_NODE: {
            word* ind_node = stack[sp];
            word* node = (word*) ind_node[1];
            stack[sp] = node;
            pc--;
            break;
          }
          default:
            printf("Unwind on %d\n", GetTag(*stack[sp]));
            exit(EXIT_FAILURE);
        }
        break;
      }
      case SLIDE: {
        word* node = stack[sp];
        int n = program[pc++];
        if (n > sp) {
          sp = 0;
        } else {
          sp = GtoAIndex(n);
        }
        stack[sp] = node;
        break;
      }
      case JUMP:
        pc = program[pc];
        break;
      case UPDATE: {
        word* node = stack[sp];
        word* ind_node = allocate(IND_NODE, 1);
        ind_node[1] = (word) node;
        int n = program[pc++];
        int temp;
        if (n > sp)
        {
          temp = 0;
        } else {
          temp = GtoAIndex(n);
        }
        stack[temp] = ind_node;
        break;
      }
      case POP: {
        int n = program[pc++];
        if (n > sp)
        {
          sp = 0;
        } else {
          sp = sp - n;
        }
        break;
      }
      default:
        printf("Illegal instruction %d at address %d\n", program[pc-1], pc-1);
        exit(EXIT_FAILURE);
    }
  }

  return 1;
}

int execute(char* filename) {
  int* program = read_file(filename);
  word** stack = (word**)malloc(sizeof(word*) * STACK_SIZE);
  init_heap();

  return execute_instructions(program, stack);
}

int main(int argc, char* argv[]) {
  int fileIndex = 1;
  int runValid = 1;

  if (argc == 2) {
  } else if (argc == 3 && strcmp(argv[1], "--verbose") == 0) {
    fileIndex = 2;
    verbose = 1;
  } else {
    runValid = 0;
  }

  if (runValid) {
    printf("Result: %d\n", execute(argv[fileIndex]));
    return 0;
  } else {
    printf("You need to pass a file to the stack machine.\n");
    printf("Usage: stack_machine [--verbose] <program>\n");
    exit(EXIT_SUCCESS);
  }
}
