#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"

#define STACK_SIZE 1000
#define LAST_INSTRUCTION -1

#define PUSHGLOBAL 0
#define PUSH 1
#define PUSHINT 2
#define MKAP 3
#define UNWIND 4
#define SLIDE 5
#define JUMP 6

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

int execute_instructions(int* program, int* stack) {
  int sp = -1;
  int pc = 0;

  for (;;) {
    switch (program[pc++]) {
      case PUSHGLOBAL:
        break;
      case PUSH:
        break;
      case PUSHINT:
        stack[sp + 1] = program[pc];
        break;
      case MKAP: {
        int left = stack[sp];
        int right = stack[sp-1];
        // The stack needs to place the application node on top of the stack.
        break;
      }
      case UNWIND:
        break;
      case SLIDE:
        break;
      case JUMP:
        program[pc] = program[pc + 1];
        pc++;
        break;
      default:
        // printf("Illegal instruction %d at address %d\n", program[pc-1],
        // pc-1);
        exit(EXIT_FAILURE);
    }
  }

  return 1;
}

int execute(char* filename) {
  int* program = read_file(filename);
  int* stack = (int*)malloc(sizeof(int) * STACK_SIZE);

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
