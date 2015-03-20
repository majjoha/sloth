#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "stack_machine.h"

/*

NOTES

The first instruction in a supercombinator is the number of input arguments to the supercombinator.

The first instruction in the program is always Unwind. This used when unwinding a new stack after eval.

dp points to the current dump item. The current dump item contains information
about the previous stack context.

*/

word* heap;
word* afterHeap;
word* lastFreeHeapNode;
int verbose = 0;

word make_header(unsigned int tag, unsigned int length, unsigned int color) { 
  return (tag << 24) | (length << 2) | color;
}

dump_item make_dump_item(unsigned int pc, unsigned int sd, unsigned int bp) {
  return (pc << 24) | (sd << 12) | bp;
}

int unbox_integer(word* word) {
  return word[1];
}

void init_heap() {
  heap = (word*)malloc(sizeof(word)*HEAP_SIZE);
  afterHeap = &heap[HEAP_SIZE];
  lastFreeHeapNode = &heap[0];
}

word* allocate(unsigned int tag, unsigned int length) {
  *lastFreeHeapNode = make_header(tag, length, Blue);
  word* heapNode = lastFreeHeapNode;
  lastFreeHeapNode = lastFreeHeapNode + length + 1;
  return heapNode;
}

int execute_instructions(int* program, word** stack, dump_item* dump) {
  int sp = -1;
  int pc = 1;
  int dp = -1;

  for (;;) {
    if (verbose) {
      int vpc = pc;
      print_stack(sp, stack);
      printf("\nPC: %d\t", pc);
      printf("Current instruction: ");
      print_instruction(program, &vpc);
      printf("\n\n*************************************\n\n");
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
        word* node = stack[GtoAIndex(next)];
        stack[++sp] = node;
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
            if (dp == 0) {
              word* integer_node = stack[sp];
              return integer_node[1];
            }

            pc = GetPc(dump[dp--]);
            break;
          }  
          case APP_NODE: {
            word* app_node = stack[sp];
            stack[++sp] = (word*) app_node[1];
            pc--;
            break;
          }
          case GLOBAL_NODE: {
            // load the code of the function
            word* global_node = stack[sp];
            pc = (int) global_node[1];

            // rearrange stack
            for (int i = 0; i < program[pc]; i++)
            {
              word* app_node = stack[sp-(i+1)];
              stack[sp-i] = (word*) app_node[2];
            }
            pc++;
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
          sp = GetCurrentBp();
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
        // get node from tip of spine
        word* node = stack[sp--];
        int n = program[pc++];
        int temp;
        // find update target
        if (n > sp)
        {
          temp = GetCurrentBp();
        } else {
          temp = GtoAIndex(n);
        }
        word* old_node = stack[temp];
        // convert old node into indirection node
        *old_node = make_header(IND_NODE, 1, Blue);
        // set indirection node to point to the node from the tip of the spine
        old_node[1] = (word) node;
        break;
      }
      case POP: {
        int n = program[pc++];
        if (n > sp)
        {
          sp = GetCurrentBp();
        } else {
          sp = sp - n;
        }
        break;
      }
      case ALLOC: {
        int n = program[pc++];
        for (int i = 0; i < n; i++)
        {
          word* ind_node = allocate(IND_NODE, 1);
          word* null_node = allocate(NULL_NODE, 0);
          ind_node[1] = (word) null_node;
          stack[++sp] = ind_node;
        }
        break;
      }
      case EVAL: {
        int new_bp, new_sd;

        if (dp == -1) {
          new_bp = 0;
          new_sd = sp;
        } else {
          new_bp = GetBp(dump[dp]) + GetSd(dump[dp]);
          new_sd = sp - new_bp;
        }

        dump_item di = make_dump_item(pc, new_sd, new_bp);
        dump[++dp] = di;
        // go to global unwind
        pc = 0;
        break;
      }
      case ADD: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 1);
        integer_node[1] = a + b;
        stack[sp] = integer_node;
        break;
      }
      case SUB: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 1);
        integer_node[1] = b - a;
        stack[sp] = integer_node;
        break;
      }
      case MUL: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 1);
        integer_node[1] = a * b;
        stack[sp] = integer_node;
        break;
      }
      case EQ: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 1);
        integer_node[1] = (a == b);
        stack[sp] = integer_node;
        break;
      }
      case JFALSE: {
        int condition = unbox_integer(stack[sp--]);
        if (condition) {
          pc++;
        } else {
          pc = program[pc];
        }
        break;
      }
      case LABEL: {
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
  dump_item* dump = (dump_item*)malloc(sizeof(dump_item) * DUMP_SIZE);
  init_heap();

  return execute_instructions(program, stack, dump);
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
