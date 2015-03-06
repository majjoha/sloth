#ifndef STACK_MACHINE_H
#define STACK_MACHINE_H

#define STACK_SIZE 1000
#define HEAP_SIZE  1000
#define LAST_INSTRUCTION -1

#define PUSHGLOBAL 0
#define PUSH       1
#define PUSHINT    2
#define MKAP       3
#define UNWIND     4
#define SLIDE      5
#define JUMP       6
#define UPDATE     7
#define POP        8

#define APP_NODE     0
#define INTEGER_NODE 1
#define GLOBAL_NODE  2
#define IND_NODE 3

#define White 0
#define Grey  1
#define Black 2
#define Blue  3

#define GetTag(h) (h>>24)
#define GtoAIndex(i) sp-(i)

typedef unsigned int word;

#endif
