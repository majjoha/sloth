#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"

#define STACK_SIZE 1000

#define PUSHGLOBAL 0
#define PUSH 1
#define PUSHINT 2
#define MKAP 3
#define UNWIND 4
#define SLIDE 5
#define JUMP 6

void print_instruction(int instruction) {
  switch (instruction) {
    case PUSHGLOBAL: printf("PUSHGLOBAL\n"); break;
    case PUSH:       printf("PUSH\n"); break;
    case PUSHINT:    printf("PUSHINT\n"); break;
    case MKAP:       printf("MKAP\n"); break;
    case UNWIND:     printf("UNWIND\n"); break;
    case SLIDE:      printf("SLIDE\n"); break;
    case JUMP:       printf("JUMP\n"); break;
    default:         printf("<unknown>\n");
  }
}

int execute_instructions(int* program, int* stack) {
  return 1;
}

int execute(char* filename) {
  int* program = read_file(filename);
  int* stack = (int*)malloc(sizeof(int)*STACK_SIZE);

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
