#ifndef STACK_MACHINE_H
#define STACK_MACHINE_H

#define STACK_SIZE 1000
#define HEAP_SIZE  10000
#define DUMP_SIZE  100
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
#define ALLOC      9
#define EVAL       10
#define ADD        11
#define SUB        12
#define MUL        13
#define DIV        14
#define NEG        15
#define EQ         16
#define NE         17
#define LE         18
#define LT         19
#define GE         20
#define GT         21
#define JFALSE     22
#define LABEL      23
#define PACK       24
#define SPLIT      25
#define CASEJUMP   26
#define PRINT      27

#define APP_NODE     0
#define INTEGER_NODE 1
#define GLOBAL_NODE  2
#define IND_NODE 3
#define NULL_NODE 4
#define CONSTR_NODE 5
#define PACK_NODE 6

#define GtoAIndex(i) sp-(i)
#define GetPc(di) ((di)>>22)
#define GetSd(di) (((di)>>11)&0x7ff)
#define GetBp(di) ((di)&0x7ff)
#define GetCurrentBp() GetBp(dump[dp])

typedef unsigned int dump_item;

#endif
