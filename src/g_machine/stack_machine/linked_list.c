#include "linked_list.h"

list_node* create_node(int key) {
  list_node* n = (list_node*)malloc(sizeof(list_node));
  n->key = key;
  n->next = NULL;
  return n;
}

list_node* get_node(list_node* n, int key) {
  if (n == NULL) {
    return NULL;
  }
  if (n->key == key) {
    return n;
  }
  return get_node(n->next, key);
}

int contains_key(list_node* n, int key) {
  if (get_node(n, key) == NULL) {
    return FALSE;
  }

  return TRUE;
}

list_node* put_key(list_node* n, int key) {
  list_node* new_node = create_node(key);
  new_node->next = n;
  return new_node;
}

list_node* remove_key(list_node* n, int key) {
  if (n == NULL) {
    return NULL;
  }
  if (n->key == key) {
    return n->next;
  }
  n->next = remove_key(n->next, key);
  return n;
}