
#include "vector.h"

void vector_init(Vector *vector) {
    // initialize size and capacity
    vector->size = 1;
    vector->capacity = VECTOR_INITIAL_CAPACITY;
    
    // allocate memory for vector->data
    vector->data = (char **) malloc(sizeof(char*) * vector->capacity);
    
}

void vector_append(Vector *vector, char* value) {
    // make sure there's room to expand into
    vector_double_capacity_if_full(vector);
    // append the value and increment vector->size
    vector->data[vector->size] = (char*) malloc(sizeof(char)*strlen(value)+1);
    strcpy(vector->data[vector->size],value);
    vector->size = vector->size +1;
    
}

char* vector_get(Vector *vector, int index) {
    if (index >= vector->size || index < 0) {
        printf("Index %d out of bounds for vector of size %d\n", index, vector->size);
        exit(1);
    }
    return vector->data[index];
}

void vector_set(Vector *vector, int index, char* value) {
    // null fill the vector up to the desired index
    while (index >= vector->size) {
        vector_append(vector, NULL);
    }
    
    // set the value at the desired index
    vector->data[index] = (char*) malloc(sizeof(char)*strlen(value)+1);
    strcpy(vector->data[index], value);
}

void vector_double_capacity_if_full(Vector *vector) {
    if (vector->size >= vector->capacity) {
        // double vector->capacity and resize the allocated memory accordingly
        vector->capacity *= 2;
        vector->data = realloc(vector->data, sizeof(char*) * vector->capacity);
    }
    
}

void vector_free(Vector *vector) {
    free(vector->data);
}

void print_vector(Vector *vector, FILE *f){
    int i;
    for(i=1; i<vector->size; i++){
        fprintf(f, "%s", vector_get(vector, i));
    }
}

void print_vector2(Vector *vector){
    int i;
    printf("VECTOR PRINT2 START\n");
    for(i=1; i<vector->size; i++){
        printf( "%s", vector_get(vector, i));
    }
}