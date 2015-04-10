#include <stdio.h>
#include <stdlib.h>
#include "stack_machine.h"
#include "../../shared/read_file.h"
#include "../../shared/mm/memory.h"

typedef unsigned char small_bool;

word* heap;
word* afterHeap;
word* lastFreeHeapNode;

word* allocate(unsigned int tag, unsigned int length) {
  return allocate_block(tag, length, &lastFreeHeapNode);
}

void copy_env(word** env, int ep, word* clos_node)
{
  for (int j = 0; j < ep+1; j++) {
    clos_node[j+2] = (word) env[j];
  }
}

void execute_instructions(int* program, word** stack, word** env, small_bool* update_markers) {
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

        if (update_markers[sp+1])
        {
          // var 2 rule

          // copy current env to closure
          copy_env(env, ep, node);

          // make nodes pc pointer point to current TAKE instruction
          node[1] = --pc;
        }
        else
        {
          // app 2 rule
          env[++ep] = node;
        }

        break;
      }
      case ENTER: {
        // var 1 rule
        int u = program[pc++];
        word* node = env[DBToAIndex(u)];

        // jump to code for closure
        pc = node[1];

        // copy closure env to global env
        int env_length = GetLength(node[0]) - 1;
        for (int i = 0; i < env_length; i++)
        {
          env[i] = (word*) node[i+2];
        }

        // set ep to the size of the closure env
        ep = env_length - 1;

        // push update marker to stack
        stack[++sp] = node;
        update_markers[sp] = TRUE;
        break;
      }
      case PUSH: {
        int u = program[pc++];
        word* node = env[DBToAIndex(u)];
        stack[++sp] = node;
        update_markers[sp] = FALSE;
        break;
      }
      case SEP: {
        break;
      }
      case LET: {
        int n = program[pc++];
        int* pc_pointers = malloc(sizeof(int)*n);

        for (int i = 0; i < n; i++) {
          pc_pointers[i] = pc;

          // We iterate over instructions until we meet the SEP instruction
          // in order to find the number of instructions for the binding.
          while (program[pc++] != SEP);

          // Step over the SEP instruction
          pc++;

          word* ind_node = allocate(IND_NODE, 1);
          word* null_node = allocate(NULL_NODE, 0);
          ind_node[1] = (word) null_node;
          env[++ep] = ind_node;
        }

        for (int i = 0; i < n; i++) {
          // ep + 1 = Size of environment
          // 1 = One word for saving the PC pointer
          int l = ep + 1 + 1;
          word* clos_node = allocate(CLOS_NODE, l);

          // Save PC pointer in first word
          clos_node[1] = pc_pointers[i];

          // Copy env to closure
          copy_env(env, ep, clos_node);

          word* ind_node = env[ep-(n-i)+1];
          ind_node[1] = (word) clos_node;
        }

        break;
      }
    }
  }
}

int execute(char* filename) {
  int* program = read_file(filename);
  word** stack = (word**)malloc(sizeof(word*) * STACK_SIZE);
  small_bool* update_markers = malloc(sizeof(small_bool*) * STACK_SIZE);
  word** env = (word**)malloc(sizeof(word*) * ENV_SIZE);
  init_heap(&heap, &afterHeap, &lastFreeHeapNode, HEAP_SIZE);

  execute_instructions(program, stack, env, update_markers);

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
