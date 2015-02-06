#include <stdio.h>
#include "ti_node.h"

void print_ti_node(ti_node_t* node) {
  switch (node->type) {
    case NUM:
      printf("NUM\n");
      break;
    case SC:
      printf("SC\n");
      break;
    case APP:
      printf("APP\n");
      break;
    case PRIM:
      printf("PRIM\n");
      break;
  }
}
