#include "utils.h"

int* read_file(char* filename) {
  int capacity = 1, size = 0;
  int* program = (int*)malloc(sizeof(int) * capacity);
  FILE* source = fopen(filename, "r");
  int instructions;

  if (source == NULL) {
    printf("File %s does not exist. Please, try again with an existing file.\n",
           filename);
    exit(EXIT_SUCCESS);
  }

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

