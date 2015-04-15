#include <stdio.h>
#include <stdlib.h>
#include "stack_machine.h"
#include "../../shared/read_file.h"
#include "../../shared/mm/memory.h"
#include "../../shared/utils.h"

typedef unsigned char small_bool;

word* heap;
word* afterHeap;
word* lastFreeHeapNode;

word* allocate(unsigned int tag, unsigned int length) {
  word* heap_address = allocate_block(tag, length, &lastFreeHeapNode);
  printf("Heap address: %d\n", address_to_heap_index(heap_address, heap));
  return heap_address;
}

void copy_env(word** env, int ep, word* clos_node)
{
  clos_node[2] = ep+1;

  for (int j = 0; j < ep+1; j++) {
    clos_node[j+3] = (word) env[j];
  }
}

void initialize_scs(int* program, word** env, int* ep, int* pc)
{
  int scs_count = program[(*pc)++];

  for (int i = 0; i < scs_count; i++) {
    word* ind_node = allocate(IND_NODE, 1);
    word* null_node = allocate(NULL_NODE, 0);
    ind_node[1] = (word) null_node;
    env[++(*ep)] = ind_node;
  }

  for (int i = 0; i < scs_count; i++) {
    // ep + 1 = Size of environment
    // 1 = One word for saving the PC pointer and one word for saving env length
    int l = *ep + 1 + 2;
    word* clos_node = allocate(CLOS_NODE, l);

    // Save PC pointer in first word
    clos_node[1] = program[(*pc)++];

    // Copy env to closure
    copy_env(env, *ep, clos_node);

    word* ind_node = env[(*ep)-(scs_count-i)+1];
    ind_node[1] = (word) clos_node;
  }
}

void execute_instructions(int* program, word** stack, word** env, small_bool* update_markers) {
  int sp = -1;
  int ep = -1;
  int pc = 0;
  initialize_scs(program, env, &ep, &pc);
  printf("PC after SCs init: %d\n", pc);

  for (;;) {
    printf("PC before switch: %d\n", pc);
    if (ep > 990) printf("EP: %d\n", ep);
    printf("SP: %d\n", sp);
    switch(program[pc++]) {
      case TAKE: {
        if (sp == -1) {
          printf("Reached the end of the program with pc %d\n", pc);
          return;
        }

        word* node = stack[sp--];

        printf("Update marker: %d\n", update_markers[sp+1]);

        if (update_markers[sp+1])
        {
          // var 2 rule
          printf("Var 2 rule\n");
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
        // TODO: Are we sure that node is always IND_NODE pointing to a CLOS_NODE?
        word* clos_node = (word*) node[1];
        pc = clos_node[1];

        // copy closure env to global env
        int env_length = clos_node[2];
        for (int i = 0; i < env_length; i++)
        {
          env[i] = (word*) clos_node[i+3];
        }

        // set ep to the size of the closure env
        ep = env_length - 1;

        // push update marker to stack
        stack[++sp] = clos_node;
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
        // LET n m 1 2 3 instrs SEP m 1 2 3 instrs SEP ... n
        int n = program[pc++];

        for (int i = 0; i < n; i++) {
          word* ind_node = allocate(IND_NODE, 1);
          word* null_node = allocate(NULL_NODE, 0);
          ind_node[1] = (word) null_node;
          env[++ep] = ind_node;
        }

        for (int i = 0; i < n; i++) {
          // read in the trimmer
          int trimmer_length = program[pc++];

          // ep + 1 = Size of environment
          // 2 = A word for saving the PC pointer and a word for saving the env size
          // trimmer_length = the trimmer
          int l = ep + 1 + 2 + trimmer_length;
          word* clos_node = allocate(CLOS_NODE, l);

          // Save PC pointer in first word
          clos_node[1] = pc;

          // Copy env to closure
          copy_env(env, ep, clos_node);

          // save trimmer in closure
          for (int j = 0; j < trimmer_length; j++) {
            clos_node[j+ep+4] = program[pc++];
          }

          word* ind_node = env[ep-(n-i)+1];
          ind_node[1] = (word) clos_node;

          // We iterate over instructions until we meet the SEP instruction
          // in order to find the number of instructions for the binding.
          while (program[pc++] != SEP);
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
