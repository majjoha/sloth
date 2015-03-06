#include "utils.h"

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
    default: printf("<unknown> ");
  }
}

void print_stack(int sp, word** stack) {
  if (sp < 0) return;

  for (int i = 0; i != sp+1; i++) {
    printf("%d\n", GetTag(*stack[i]));
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
