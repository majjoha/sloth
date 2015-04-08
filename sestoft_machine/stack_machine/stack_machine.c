#include <stdio.h>
#include <stdlib.h>
#include "stack_machine.h"
#include "../../shared/read_file.h"
#include "../../shared/mm/memory.h"

word* heap;
word* afterHeap;
word* lastFreeHeapNode;

word* allocate(unsigned int tag, unsigned int length) {
  return allocate_block(tag, length, &lastFreeHeapNode);
}

void execute_instructions(int* program, word** stack, word** env) {
  int sp = -1;
  int pc = 0;
  int ep = -1;

  for (;;) {
    switch(program[pc++]) {
      case TAKE: {
        if (sp == -1) {
          printf("Reached the end of the program with pc %d\n", pc);
          return;
        }

        word* node = stack[sp--];
        env[++ep] = node;
        break;
      }
      case ENTER: {
        break;
      }
      case PUSH: {
        int u = program[pc++];
        word* node = env[DBToAIndex(u)];
        stack[++sp] = node;
        break;
      }
      case SEP: {
        break;
      }
      case LET: {
        int n = program[pc++];
        int** instructions = malloc(sizeof(int**)*n);
        int* length = malloc(sizeof(int*)*n);
        int initial_pc;

        for (int i = 0; i < n; i++) {
          initial_pc = pc;

          // We iterate over instructions until we meet the SEP instruction
          // in order to find the number of instructions for the binding.
          while (program[pc++] != SEP);
          length[i] = pc - initial_pc;
          instructions[i] = malloc(sizeof(int*)*length[i]);

          // Initialize the array with all the instructions for the binding.
          for (int j = 0; j < length[i]; j++) {
            instructions[i][j] = program[initial_pc+j];
          }

          // Step over the SEP instruction
          pc++;

          word* ind_node = allocate(IND_NODE, 1);
          word* null_node = allocate(NULL_NODE, 0);
          ind_node[1] = (word) null_node;
          env[++ep] = ind_node;
        }

        for (int i = 0; i < n; i++) {
          // length[i] = Number of instructions
          // ep + 1 = Size of environment
          // 1 = One word for saving the size of the environment
          int l = length[i] + ep + 1 + 1;
          word* clos_node = allocate(CLOS_NODE, l);
          clos_node[1] = ep + 1;

          for (int j = 0; j < ep+1; j++) {
            clos_node[j+2] = (word) env[j];
          }

          for (int k = 0; k < length[i]; k++) {
            clos_node[k+ep+3] = instructions[i][k];
          }

          word* ind_node = env[ep-(n-i)+1];
          ind_node[1] = (word) clos_node;
        }

        break;
      }
      case ENTERGLOBAL: {
        break;
      }
      case PUSHGLOBAL: {
        break;
      }
    }
  }
}

int execute(char* filename) {
  int* program = read_file(filename);
  word** stack = (word**)malloc(sizeof(word*) * STACK_SIZE);
  word** env = (word**)malloc(sizeof(word*) * ENV_SIZE);
  init_heap(&heap, &afterHeap, &lastFreeHeapNode, HEAP_SIZE);

  execute_instructions(program, stack, env);

  return 0;
}

int main(int argc, char* argv[]) {
  int fileIndex = 1;
  int runValid = 1;

  if (argc != 2) {
    runValid = 0;
  }

  if (runValid) {
    execute(argv[fileIndex]);
    return 0;
  } else {
    printf("You need to pass a file to the stack machine.\n");
    printf("Usage: stack_machine [--verbose] <program>\n");
    exit(EXIT_SUCCESS);
  }
}
