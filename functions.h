#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "symtab.h"
#include "vector.h"


int afegir_variable(sym_name_type  name,
                    sym_value_type *value);

int agafar_variable(sym_name_type name,
                    sym_value_type *value);

char* incr_count(int *count);

char* get_count(int count);

Node* mergeLists(Node* list1, Node* list2);

void completa(Vector *v, Node* list, int line);

Node* creaLlista(int line);

void display_list(Node *n);