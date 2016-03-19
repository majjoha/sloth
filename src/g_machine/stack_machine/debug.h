#include <stdio.h>
#include <stdlib.h>
#include "memory.h"
#include "../../shared/read_file.h"
#include "stack_machine.h"
#include "linked_list.h"

#ifndef UTILS_H
#define UTILS_H

void print_instructions(int* instructions);
void print_instruction(int* instructions, int* pi);
void print_node(word* node, int tab_factor, list_node* visited_nodes);
void print_result(word* node);
void print_stack(int sp, word** stack);
char* tag_to_name(int tag);

#endif
