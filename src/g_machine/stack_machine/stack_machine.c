#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

#include "debug.h"
#include "stack_machine.h"
#include "memory.h"
#include "../../shared/utils.h"

/*

NOTES

The first instruction in a supercombinator is the number of input arguments to
the supercombinator.

The first instruction in the program is always Unwind. This used when unwinding
a new stack after eval.

dp points to the current dump item. The current dump item contains information
about the previous stack context.

*/

int verbose = 0;
int manualStep = 0;
int sp;
word** stack;

dump_item make_dump_item(unsigned int pc, unsigned int sd, unsigned int bp) {
  return (pc << 22) | (sd << 11) | bp;
}

word* allocate(unsigned int tag, unsigned int length) {
  return allocate_block(tag, length, stack, sp);
}

void execute_instructions(int* program, dump_item* dump) {
  sp = -1;
  int pc = 3;
  int dp = -1;

  for (;;) {
    if (verbose) {
      int vpc = pc;
      print_stack(sp, stack);
      printf("\nPC: %d\t\t", pc);
      printf("Current instruction: ");
      print_instruction(program, &vpc);
      printf("\n\n*************************************\n\n");
    }

    if (manualStep) {
      getchar();
    }

    switch (program[pc++]) {
      case PUSHGLOBAL: {
        word* global_node = allocate(GLOBAL_NODE, 2);
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
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = program[pc++];
        stack[++sp] = integer_node;
        break;
      }
      case MKAP: {
        word* left = stack[sp];
        word* right = stack[sp - 1];
        word* app_node = allocate(APP_NODE, 2);
        app_node[1] = (word)left;
        app_node[2] = (word)right;
        stack[--sp] = app_node;
        break;
      }
      case UNWIND: {
        switch (GetTag(*stack[sp])) {
          case INTEGER_NODE: {
            pc = GetPc(dump[dp--]);
            break;
          }
          case APP_NODE: {
            word* app_node = stack[sp];
            stack[++sp] = (word*)app_node[1];
            pc--;
            break;
          }
          case GLOBAL_NODE: {
            // load the code of the function
            word* global_node = stack[sp];
            pc = (int)global_node[1];

            // rearrange stack
            for (int i = 0; i < program[pc]; i++) {
              word* app_node = stack[sp - (i + 1)];
              stack[sp - i] = (word*)app_node[2];
            }
            pc++;
            break;
          }
          case IND_NODE: {
            word* ind_node = stack[sp];
            word* node = (word*)ind_node[1];
            stack[sp] = node;
            pc--;
            break;
          }
          case CONSTR_NODE: {
            pc = GetPc(dump[dp--]);
            break;
          }
          case PACK_NODE: {
            word* pack_node = stack[sp];
            int tag = pack_node[1];
            int n = pack_node[2];
            word* constr_node = allocate(CONSTR_NODE, 2 + n);
            constr_node[1] = tag;
            constr_node[2] = n;
            for (int i = 0; i < n; i++) {
              word* app_node = stack[sp - (i + 1)];
              constr_node[3 + i] = app_node[2];
            }
            // Update the root of the redex
            sp = sp-n;
            word* old_node = stack[sp];
            // convert old node into indirection node
            convert_block_to_indirection_node(old_node);
            // set indirection node to point to the constructor node
            old_node[1] = (word)constr_node;
            pc = GetPc(dump[dp--]);
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
        if (n > sp) {
          temp = GetCurrentBp();
        } else {
          temp = GtoAIndex(n);
        }
        word* old_node = stack[temp];
        // convert old node into indirection node
        convert_block_to_indirection_node(old_node);
        // set indirection node to point to the node from the tip of the spine
        old_node[1] = (word)node;
        break;
      }
      case POP: {
        int n = program[pc++];
        if (n > sp) {
          sp = GetCurrentBp();
        } else {
          sp = sp - n;
        }
        break;
      }
      case ALLOC: {
        int n = program[pc++];
        for (int i = 0; i < n; i++) {
          word* ind_node = allocate(IND_NODE, 2);
          //word* null_node = allocate(NULL_NODE, 0);
          word* null_node = NULL;
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
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = a + b;
        stack[sp] = integer_node;
        break;
      }
      case SUB: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = a - b;
        stack[sp] = integer_node;
        break;
      }
      case MUL: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = a * b;
        stack[sp] = integer_node;
        break;
      }
      case DIV: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = a / b;
        stack[sp] = integer_node;
        break;
      }
      case NEG: {
        int a = unbox_integer(stack[sp--]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = -a;
        stack[sp] = integer_node;
        break;
      }
      case EQ: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = (a == b);
        stack[sp] = integer_node;
        break;
      }
      case NE: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = (a != b);
        stack[sp] = integer_node;
        break;
      }
      case LE: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = (a <= b);
        stack[sp] = integer_node;
        break;
      }
      case LT: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = (a < b);
        stack[sp] = integer_node;
        break;
      }
      case GE: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = (a >= b);
        stack[sp] = integer_node;
        break;
      }
      case GT: {
        int a = unbox_integer(stack[sp--]);
        int b = unbox_integer(stack[sp]);
        word* integer_node = allocate(INTEGER_NODE, 2);
        integer_node[1] = (a > b);
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
      case PACK: {
        int tag = program[pc++];
        int n = program[pc++];
        word* pack_node = allocate(PACK_NODE, 2);
        pack_node[1] = tag;
        pack_node[2] = n;
        stack[++sp] = pack_node;
        break;
      }
      case SPLIT: {
        int n = program[pc++];
        word* pack_node = stack[sp];

        for (int i = n + 2; i >= 3; i--) {
          stack[sp++] = (word*)pack_node[i];
        }

        sp--;

        break;
      }
      case CASEJUMP: {
        int n = program[pc++];
        // Question: Could there be a chain of indirection nodes here? Right now we assume only 0 or 1 indirection node
        word* node = stack[sp];
        word* constr_node;
        switch GetTag(node[0]) {
          case IND_NODE: {
            word* ind_node = stack[sp];
            constr_node = (word*)ind_node[1];
            stack[sp] = constr_node;
            break;
          }
          case CONSTR_NODE: {
            constr_node = node;
            break;
          }
          default: {
            printf("Error: CASEJUMP on %s\n", tag_to_name(GetTag(node[0])));
            exit(EXIT_FAILURE);
          }
        }
        int matched = 0;
        for (int i = 0; i < n * 2; i = i + 2) {
          int tag = program[pc + i];
          int lab = program[pc + i + 1];

          if (tag == constr_node[1]) {
            pc = lab;
            matched = 1;
            break;
          }
        }

        if (!matched) {
          printf("Illegal casejump. No matching tag.\n");
          exit(EXIT_FAILURE);
        }
        break;
      }
      case PRINT: {
        word* node = stack[sp];

        switch (GetTag(*node)) {
          case INTEGER_NODE: {
            printf("%d\n", node[1]);

            if (sp == 0) {
              return;
            }

            sp--;
            pc = 1;
            break;
          }
          case CONSTR_NODE: {
            int n = node[2];

            for (int i = n + 2; i >= 3; i--) {
              stack[sp++] = (word*)node[i];
            }

            if (sp == 0) {
              return;
            }

            sp--;
            pc = 1;
            break;
          }
          case IND_NODE: {
            stack[sp] = (word*)node[1];
            // go to global print again
            pc = 2;
            break;
          }
          default: {
            printf("Illegal print on tag %d\n", GetTag(*node));
            exit(EXIT_FAILURE);
          }
        }

        break;
      }
      default:
        printf("Illegal instruction %d at address %d\n", program[pc - 1],
               pc - 1);
        exit(EXIT_FAILURE);
    }
  }
}

int execute(char* filename) {
  int* program = read_file(filename);
  stack = (word**)malloc(sizeof(word*) * STACK_SIZE);
  dump_item* dump = (dump_item*)malloc(sizeof(dump_item) * DUMP_SIZE);
  init_heap(HEAP_SIZE);

  execute_instructions(program, dump);

  return 0;
}

int main(int argc, char* argv[]) {
  int fileIndex;
  int c;

  // -v sets the verbose flag
  // -m sets the manual flag for debugging, and the verbose flag
  while ((c = getopt (argc, argv, "vm")) != -1) {
    switch (c) {
      case 'v':
        verbose = 1;
        break;
      case 'm':
        manualStep = 1;
        verbose = 1;
        break;
      case '?':
        fprintf(stderr, "Unknown option character `%c'.\n", optopt);
        return 1;
      default:
        abort();
    }
  }

  for (fileIndex = optind; fileIndex < argc; fileIndex++) {
    execute(argv[fileIndex]);
    return 0;
  }

  if (fileIndex == argc) {
    printf("You need to pass a file to the stack machine.\n");
    printf("Usage: stack_machine [-vm] <program>\n");
    exit(EXIT_SUCCESS);
  }
}
