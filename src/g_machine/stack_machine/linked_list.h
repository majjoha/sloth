#include <stdlib.h>
#define FALSE 0
#define TRUE  1

typedef struct Node {
  int key;
  struct Node* next;
} list_node;

/*
Creates a new linked list node, whose next pointer points to NULL.
*/
list_node* create_node(int key);

/*
If a node in the list contains the given key it is returned. 
Otherwise NULL is returned. If multiple nodes contains
the key, the first node is returned.
*/
list_node* get_node(list_node* n, int key);

/*
Returns true if any node in the list contains the key, otherwise
returns false.
*/
int contains_key(list_node* n, int key);

/* 
Returns a linked list with the given key added to the front of the list.
*/
list_node* put_key(list_node* n, int key);

/*
If a node in the list contains the given key it is removed from the
list and returned. Otherwise NULL is returned. If multiple nodes contains
the key, the first node is removed.
*/
list_node* remove_key(list_node* n, int key);