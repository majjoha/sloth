#include "debug.h"
#include "../../shared/utils.h"

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
    case PACK: { printf("PACK %d %d;", instructions[++(*pi)], instructions[++(*pi)]); break;}
    case SPLIT: { printf("SPLIT %d;", instructions[++(*pi)]); break;}
    case CASEJUMP: { printf("CASEJUMP;"); break;}
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
    case PACK_NODE: return "PACK_NODE";
    default: exit(EXIT_FAILURE);
  }
}

void print_node(word* node, int tab_factor, list_node* visited_nodes) {
  int tag = GetTag(*node);
  int heap_index = address_to_heap_index(node, heap);
  printf("(%d) %s: ", heap_index, tag_to_name(tag));

  if (visited_nodes == NULL)
  {
    visited_nodes = create_node(heap_index);
  }
  else if(contains_key(visited_nodes, heap_index))
  {
    printf("Already visited\n");
    return;
  }
  else
  {
    visited_nodes = put_key(visited_nodes, heap_index);
  }

  switch (tag) {
    case APP_NODE: {
      // Print left node
      printf("\n");
      for (int i = 0; i < tab_factor; i++) printf("\t");
      printf("\tLeft: ");
      print_node((word*)node[1], tab_factor+1, visited_nodes);

      // Print right node
      for (int i = 0; i < tab_factor; i++) printf("\t");
      printf("\tRight: ");
      print_node((word*)node[2], tab_factor+1, visited_nodes);
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
      printf("\n\t");
      for (int i = 0; i < tab_factor; i++) printf("\t");
      print_node((word*)node[1], tab_factor+1, visited_nodes);
      break;
    }
    case NULL_NODE: {
      printf("\n");
      break;
    }
    case PACK_NODE: {
      printf("Tag %d, Arity %d\n", node[1], node[2]);
      if (node[2] != 0 && (word*)node[3] != NULL)
      {
        // Print 1st node
        for (int i = 0; i < tab_factor; i++) printf("\t");
        printf("\t1st: ");
        print_node((word*)node[3], tab_factor+1, visited_nodes);

        // Print 2nd node
        for (int i = 0; i < tab_factor; i++) printf("\t");
        printf("\t2nd: ");
        print_node((word*)node[4], tab_factor+1, visited_nodes);
      }
      
      break;
    }
    default: { exit(EXIT_FAILURE); }
  }

  remove_key(visited_nodes, heap_index);
}

void print_result(word* node) {
  switch (GetTag(*node)) {
    case APP_NODE: {
      printf("APP_NODE\n");
      break;
    }
    case INTEGER_NODE: {
      printf("%d\n", node[1]);
      break;
    }
    case GLOBAL_NODE: {
      printf("GLOBAL_NODE\n");
      break;
    }
    case IND_NODE: {
      printf("IND_NODE\n");
      break;
    }
    case NULL_NODE: {
      printf("NULL_NODE\n");
      break;
    }
    case PACK_NODE: {
      int n = node[2];
      printf("PACK_NODE\n");
      printf("TAG: %d\n", node[1]);
      for (int i = 0; i < n; i++) {
        print_result((word*)node[3+i]);
      }
      break;
    }
  }
}

void print_stack(int sp, word** stack) {
  if (sp < 0) return;
  
  list_node* visited_nodes = NULL;
  for (int i = 0; i != sp+1; i++) {
    word* node = stack[i];
    print_node(node, 0, visited_nodes);
  }
}
