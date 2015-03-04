#include "utils.h"

int* read_file(char* filename) {
  int* program = (int*)malloc(sizeof(int)*1000);
  FILE *source = fopen(filename, "r");
  int i = 0;
  int num;

  while (fscanf(source, "%d", &num) == 1) {
    program[i] = num;
    i++;
  }

  // TODO: This needs to be fixed.
  // Insert -1 at the end of array to determine where to stop.
  program[i] = -1;

  fclose(source);

  return program;
}
