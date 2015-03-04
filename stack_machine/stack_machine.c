#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int* read_file(char* filename) {
  int capacity = 1, size = 0;
  int* program = (int*)malloc(sizeof(int) * capacity);
  FILE* source = fopen(filename, "r");
  int instructions;

  while (fscanf(source, "%d", &instructions) == 1) {
    if (size >= capacity) {
      int* buffer = (int*)malloc(sizeof(int) * 2 * capacity);
      int i;
      for (i = 0; i < capacity; i++) {
        buffer[i] = program[i];
      }
      free(program);
      program = buffer;
      capacity *= 2;
    }
    program[size++] = instructions;
  }
  fclose(source);

  return program;
}

int main(int argc, char* argv[]) {
  if (argc == 2) {
    read_file(argv[1]);
    return 1;
  } else {
    printf("You need to pass a file to the stack machine.\n");
    printf("Usage: stack_machine <program>\n");
    exit(EXIT_SUCCESS);
  }

  return 0;
}
