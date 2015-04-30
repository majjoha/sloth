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
  //printf("Heap address: %d\n", address_to_heap_index(heap_address, heap));
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

word* allocate_closure(ep, trimmer_length) {
  // ep + 1 = Size of environment
  // 2 = A word for saving the PC pointer and a word for saving the env size
  // trimmer_length = the trimmer
  int l = ep + 1 + 2 + trimmer_length;
  return allocate(CLOS_NODE, l);
}

void update_indirection_node(word* ind_node, int ep, int pc, word** env)
{
  word* clos_node = allocate_closure(ep, ep+1);

  clos_node[1] = pc;

  // Copy env to closure
  copy_env(env, ep, clos_node);

  // save trimmer in closure
  for (int j = 0; j <= ep; j++) {
    clos_node[j+ep+4] = j;
  }

  ind_node[1] = (word) clos_node;
}

void enter_closure(word* ind_node, int* pc, word** env, int* ep, word** stack, small_bool* update_markers, int* sp)
{
  word* clos_node = (word*) ind_node[1];

  *pc = clos_node[1];

  // copy closure env to global env
  int env_length = clos_node[2];
  for (int i = 0; i < env_length; i++)
  {
    env[i] = (word*) clos_node[i+3];
  }

  // set ep to the size of the closure env
  *ep = env_length - 1;

  // push update marker to stack
  stack[++(*sp)] = ind_node;
  update_markers[(*sp)] = TRUE;
}

void execute_instructions(int* program, word** stack, word** env, small_bool* update_markers, word** print_stack) {
  int sp = -1;
  int ep = -1;
  int pp = -1;
  int pc = START_PC;
  initialize_scs(program, env, &ep, &pc);
  //printf("PC after SCs init: %d\n", pc);

  for (;;) {
    //printf("PC before switch: %d\n", pc);
    //if (ep > 990) printf("EP: %d\n", ep);
    //printf("SP: %d\n", sp);
    switch(program[pc++]) {
      case TAKE: {
        if (sp == -1) {
          printf("Evaluated to TAKE with PC %d\n", pc-1);
          pc = PRINT_PC;
          break;
        }

        word* node = stack[sp--];

        //printf("Update marker: %d\n", update_markers[sp+1]);

        if (update_markers[sp+1])
        {
          // var 2 rule    
          update_indirection_node(node, ep, --pc, env);
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
        word* ind_node = env[DBToAIndex(u)];

        // jump to code for closure
        enter_closure(ind_node, &pc, env, &ep, stack, update_markers, &sp);
        break;
      }
      case PUSH: {
        int u = program[pc++];
        word* node = env[DBToAIndex(u)];
        stack[++sp] = node;
        update_markers[sp] = FALSE;
        break;
      }
      case SEPCASE: {
        printf("PC reached SEPCASE.\n");
        return;
      }
      case LET: {
        // LET d n t 1 2 3 instrs SEPLET t 1 2 3 instrs SEPLET ...
        int depth = program[pc++];

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

          word* clos_node = allocate_closure(ep, trimmer_length);

          // Copy env to closure
          copy_env(env, ep, clos_node);

          // save trimmer in closure
          for (int j = 0; j < trimmer_length; j++) {
            clos_node[j+ep+4] = program[pc++];
          }

          // Save PC pointer in first word
          clos_node[1] = pc;

          word* ind_node = env[ep-(n-i)+1];
          ind_node[1] = (word) clos_node;

          // We iterate over instructions until we meet the SEPLET instruction
          // with the correct depth in order to find the number of instructions
          // for the binding.
          while (!(program[pc++] == SEPLET && program[pc++] == depth));
        }

        break;
      }
      case CASE:
      {
        int depth = program[pc++];

        int n = program[pc++];

        // The PC that we have to jump to after allocating alts_node
        int expr_pc = pc++;

        while (!(program[pc++] == SEPCASE && program[pc++] == depth));

        int l = 1+n+ep+1;
        word* alts_node = allocate(ALTS_NODE, l);
        alts_node[1] = n;

        for (int i = 2; i <= n+1; i++)
        {
          alts_node[i] = pc;

          while (!(program[pc++] == SEPCASE && program[pc++] == depth));
        }

        // Copy env to alts_node
        // n+2 = the number of tags + (the header + n)
        for (int j = 0; j <= ep; j++) {
          alts_node[j+n+2] = (word) env[j];
        }

        update_markers[++sp] = FALSE;
        stack[sp] = alts_node;

        pc = expr_pc;
        break;
      }
      case PACK:
      {
        if (sp == -1)
        {
          printf("Evaluated to PACK with PC %d\n", pc-1);

          int tag = program[pc++];
          int arity = program[pc++];

          printf("Tag: %d\n", tag);
          printf("Arity: %d\n", arity);

          for (int i = 0; i < arity; i++) {
            word* node = env[DBToAIndex(i)];

            // Unless we are at the special Integer Pack, 
            // then the elements of the pack will always be indirection nodes
            // pointing to closures
            if (GetTag(node[0]) == INT_NODE) {
              int n = unbox_integer(node);
              printf("Result: %d\n", n);
            } else {
              print_stack[++pp] = node;
            }
          }

          pc = PRINT_PC;
          break;
        }

        if (update_markers[sp])
        {
          word* ind_node = stack[sp--];
          
          update_indirection_node(ind_node, ep, --pc, env);
        }

        else
        {
          int tag = program[pc++];
          int arity = program[pc++];

          word* alts_node = stack[sp--];

          if (GetTag(alts_node[0]) != ALTS_NODE)
          {
            printf("Expected ALTS_NODE on stack when reading PACK on program.\n");
            printf("Recieved node with tag %d\n", GetTag(alts_node[0]));
            return;
          }

          // Copy the old environment, so that we can use it to construct E''.
          word** old_env = (word**)malloc(sizeof(word*)*arity);

          for (int i = 0; i < arity; i++) {
            old_env[arity-1-i] = env[DBToAIndex(i)];
          }

          // copy alts_node env to global env
          int env_length = GetLength(alts_node[0]) - alts_node[1] - 1;
          int n = alts_node[1];
          for (int i = 0; i < env_length; i++)
          {
            env[i] = (word*) alts_node[i+n+2];
          }

          // Copy 1..a elements from the old environment into the new one.
          for (int i = 0; i < arity; i++) {
            env[i+env_length] = old_env[i];
          }

          // set ep to the size of the alts_node env
          ep = env_length + arity - 1;

          pc = alts_node[tag+1];
        }
        break;
      }
      case CSTI:
      {
        int n = program[pc++];

        word* int_node = allocate(INT_NODE, 1);

        int_node[1] = n;

        env[0] = int_node;

        ep = 0;

        pc = INT_PC;
        break;
      }
      case ADD:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        word* int_node = allocate(INT_NODE, 1);
        int_node[1] = a + b;

        env[0] = int_node;
        ep = 0;

        pc = INT_PC;
        break;
      }
      case SUB:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        word* int_node = allocate(INT_NODE, 1);
        int_node[1] = b - a;

        env[0] = int_node;
        ep = 0;

        pc = INT_PC;
        break;
      }
      case MUL:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        word* int_node = allocate(INT_NODE, 1);
        int_node[1] = a * b;

        env[0] = int_node;
        ep = 0;

        pc = INT_PC;
        break;
      }
      case DIV:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        word* int_node = allocate(INT_NODE, 1);
        int_node[1] = b / a;

        env[0] = int_node;
        ep = 0;

        pc = INT_PC;
        break;
      }
      case LT:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        ep = -1;

        if (b < a) pc = TRUE_PC;
        else pc = FALSE_PC;

        break;
      }
      case GT:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        ep = -1;

        if (b > a) pc = TRUE_PC;
        else pc = FALSE_PC;

        break;
      }
      case LE:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        ep = -1;

        if (b <= a) pc = TRUE_PC;
        else pc = FALSE_PC;

        break;
      }
      case GE:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        ep = -1;

        if (b >= a) pc = TRUE_PC;
        else pc = FALSE_PC;

        break;
      }
      case EQ:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        ep = -1;

        if (a == b) pc = TRUE_PC;
        else pc = FALSE_PC;

        break;
      }
      case NEG:
      {
        int a = unbox_integer(env[ep--]);

        word* int_node = allocate(INT_NODE, 1);
        int_node[1] = -a;

        env[0] = int_node;
        ep = 0;

        pc = INT_PC;
        break;
      }
      case SEPLET:
      {
        printf("PC reached SEPLET.\n");
        return;
      }
      case NEQ:
      {
        int a = unbox_integer(env[ep--]);
        int b = unbox_integer(env[ep--]);

        ep = -1;

        if (a != b) pc = TRUE_PC;
        else pc = FALSE_PC;

        break;
      }
      case PRINT:
      {
        if (pp == -1) {
          return;
        }

        word* ind_node = print_stack[pp--];

        if (GetTag(ind_node[0]) != IND_NODE) {
          printf("Element on print stack is not indirection node\n");
          return;
        }

        enter_closure(ind_node, &pc, env, &ep, stack, update_markers, &sp);
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
  word** print_stack = (word**)malloc(sizeof(word*) * PRINT_STACK_SIZE);
  init_heap(&heap, &afterHeap, &lastFreeHeapNode, HEAP_SIZE);

  execute_instructions(program, stack, env, update_markers, print_stack);

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
