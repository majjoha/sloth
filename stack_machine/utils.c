#include "utils.h"

extern word* heap;

void print_instructions(int* instructions) {
  for (int i = 0; instructions[i] != LAST_INSTRUCTION; i++) {
    print_instruction(instructions, &i);
  }
  printf("\n");
}

void print_instruction(int* instructions, int* pi) {
  switch (instructions[*pi]) {
    case PUSHGLOBAL: { printf("PUSHGLOBAL %d; ", instructions[++(*pi)]); break; }
    case PUSH: { printf("PUSH %d; ", instructions[++(*pi)]); break; }
    case PUSHINT: { printf("PUSHINT %d; ", instructions[++(*pi)]); break; }
    case MKAP: { printf("MKAP; "); break; }
    case UNWIND: { printf("UNWIND; "); break; }
    case SLIDE: { printf("SLIDE %d; ", instructions[++(*pi)]); break; }
    case JUMP: { printf("JUMP %d; ", instructions[++(*pi)]); break; }
    case UPDATE: { printf("UPDATE %d; ", instructions[++(*pi)]); break;}
    case POP: { printf("POP %d; ", instructions[++(*pi)]); break;}
    case ALLOC: { printf("ALLOC %d; ", instructions[++(*pi)]); break;}
    case EVAL: { printf("EVAL; "); break;}
    case ADD: { printf("ADD; "); break;}
    case SUB: { printf("SUB; "); break;}
    case MUL: { printf("MUL; "); break;}
    case DIV: { printf("DIV; "); break;}
    case NEG: { printf("NEG; "); break;}
    case EQ: { printf("EQ; "); break;}
    case NE: { printf("NE; "); break;}
    case LE: { printf("LE; "); break;}
    case LT: { printf("LT; "); break;}
    case GE: { printf("GE; "); break;}
    case GT: { printf("GT; "); break;}
    case JFALSE: { printf("JFALSE %d;", instructions[++(*pi)]); break;}
    case LABEL: { printf("LABEL; "); break;}
    default: printf("<unknown> ");
  }
}

char* tag_to_name(int tag) {
  switch (tag) {
    case APP_NODE: return "APP NODE";
    case INTEGER_NODE: return "INTEGER NODE";
    case GLOBAL_NODE: return "GLOBAL NODE";
    case IND_NODE: return "INDIRECTION NODE";
    case NULL_NODE: return "NULL NODE";
    default: exit(EXIT_FAILURE);
  }
}

int address_to_heap_index(word* node) {
  return node - heap;
}

void print_node(word* node, int tab_factor) {
  int tag = GetTag(*node);
  printf("(%d) %s: ", address_to_heap_index(node), tag_to_name(tag));

  switch (tag) {
    case APP_NODE: {
      // Print left node
      printf("\n");
      for (int i = 0; i < tab_factor; i++) printf("\t");
      printf("\tLeft: ");
      print_node((word*)node[1], tab_factor+1);

      // Print right node
      for (int i = 0; i < tab_factor; i++) printf("\t");
      printf("\tRight: ");
      print_node((word*)node[2], tab_factor+1);
      break;
    }
    case INTEGER_NODE: {
      printf("%d\n", node[1]);
      break;
    }
    case GLOBAL_NODE: {
      printf("%d\n", node[1]);
      break;
    }
    case IND_NODE: {
      for (int i = 0; i < tab_factor; i++) printf("\t");
      print_node((word*)node[1], tab_factor+1);
      printf("\n");
      break;
    }
    case NULL_NODE: {
      printf("\n");
      break;
    }
    default: { exit(EXIT_FAILURE); }
  }
}

void print_stack(int sp, word** stack) {
  if (sp < 0) return;

  for (int i = 0; i != sp+1; i++) {
    word* node = stack[i];
    print_node(node, 0);
  }
}

int* read_file(char* filename) {
  int* program = (int*)malloc(sizeof(int)*1000);
  FILE *source = fopen(filename, "r");
  int i = 0;
  int num;

  while (fscanf(source, "%d", &num) == 1) {
    program[i] = num;
    i++;
  }

  // Insert -1 at the end of array to determine where to stop.
  program[i] = -1;

  fclose(source);

  return program;
}
