 #include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VECTOR_INITIAL_CAPACITY 100

// Define a vector type
typedef struct {
    int size;      // slots used so far
    int capacity;  // total available slots
    char **data;     // array of char pointers we're storing
} Vector;

void vector_init(Vector *vector);

void vector_append(Vector *vector, char* value);

char* vector_get(Vector *vector, int index);

void vector_set(Vector *vector, int index, char* value);

void vector_double_capacity_if_full(Vector *vector);

void vector_free(Vector *vector);

void print_vector(Vector *vector, FILE *f);

void print_vector2(Vector *vector);