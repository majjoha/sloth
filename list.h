/******************************************************************************
list.h

Header file with definition of a simple linked list.

******************************************************************************/

#ifndef _LIST_H
#define _LIST_H

/* structures */
typedef struct node {
    void *elm;
    /* use void type for generality; we cast the element's type to void type */
    struct node *next;
} node_t;

typedef struct list {

    int len;
    node_t *first;
    node_t *last;
} list_t;

/* functions */
list_t *list_new(void);

/* return a new list structure */
void list_add(list_t *l, node_t *n);

/* add node n to list l as the last element */
node_t *list_remove(list_t *l);

/* remove and return the first element from list l*/
node_t *node_new(void);

node_t *node_new_anything(void* anything);

/* return a new node structure */
node_t *node_new_str(char *s);     /* return a new node structure, where elm points to new copy of string s */

#endif
