#ifndef STACK_MACHINE_H
#define STACK_MACHINE_H

#define TRUE  1
#define FALSE 0

#define STACK_SIZE 1000
#define HEAP_SIZE  10000
#define ENV_SIZE   1000
#define PRINT_STACK_SIZE 1000

#define TAKE         0
#define ENTER        1
#define PUSH         2
#define SEPCASE     -3
#define LET          4
#define ENTERGLOBAL  5
#define PUSHGLOBAL   6
#define CASE         7
#define PACK         8
#define CSTI         9
#define ADD         10
#define SUB         11
#define MUL         12
#define DIV         13
#define LT          14
#define GT          15
#define LE          16
#define GE          17
#define EQ          18
#define NEG         19
#define SEPLET     -20
#define NEQ         21
#define PRINT       22

#define IND_NODE  0
#define NULL_NODE 1
#define CLOS_NODE 2
#define ALTS_NODE 3
#define INT_NODE  4

#define INT_PC    0
#define FALSE_PC  3
#define TRUE_PC   6
#define PRINT_PC  9
#define START_PC 10

#define DBToAIndex(i) ep-(i)

#endif
