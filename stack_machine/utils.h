#include <stdio.h>
#include <stdlib.h>
#include "stack_machine.h"

#ifndef UTILS_H
#define UTILS_H

void print_instructions(int* instructions);
void print_instruction(int* instructions, int* pi);
void print_stack(int sp, word** stack);
int* read_file(char* filename);

#endif
