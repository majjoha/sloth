/******************************************************************************
list.c

Implementation of simple linked list defined in list.h.

******************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "list.h"

/* list_new: return a new list structure */
list_t *list_new(void) {
    list_t *l;

    l = (list_t *) malloc(sizeof(list_t));
    l->len = 0;

    /* insert root element which should never be removed */
    l->first = l->last = (node_t *) malloc(sizeof(node_t));
    l->first->elm = NULL;
    l->first->next = NULL;
    return l;
}

/* list_add: add node n to list l as the last element */
void list_add(list_t *l, node_t *n) {

    node_t *last_node = l->last;
    last_node->next = n;
    l->last = n;
    l->len++;

}

/* list_remove: remove and return the first (non-root) element from list l */
node_t *list_remove(list_t *l) {
    if (l->first->next) {
        node_t *first_node = l->first->next;
        l->first->next = first_node->next;
        l->len--;
        if (l->len == 0) {
            l->last = l->first;
        }
        return first_node;
    }
    return NULL;
}

/* node_new: return a new node structure */
node_t *node_new(void) {
    node_t *n;
    n = (node_t *) malloc(sizeof(node_t));
    n->elm = NULL;
    n->next = NULL;
    return n;
}

node_t *node_new_anything(void* anything) {
    node_t *n;
    n = (node_t *) malloc(sizeof(node_t));
    n->elm = anything;
    n->next = NULL;
    return n;
}

/* node_new_str: return a new node structure, where elm points to new copy of s */
node_t *node_new_str(char *s) {
    node_t *n;
    n = (node_t *) malloc(sizeof(node_t));
    n->elm = (void *) malloc((strlen(s) + 1) * sizeof(char));
    strcpy((char *) n->elm, s);
    n->next = NULL;
    return n;
}
