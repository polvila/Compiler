#include "functions.h"

int afegir_variable(sym_name_type  name,
                sym_value_type *value){
    
    return sym_enter(name,value);
    
}

int agafar_variable(sym_name_type name,
                sym_value_type *value){
    
    
    return sym_lookup(name,value);
    
    
}

char* incr_count(int *count){
    *count = *count + 1;
    return get_count(*count);
}

char* get_count(int count){
    char* string;
    if(count < 10){
        string = (char *)malloc(sizeof(char)*floor(log10(abs(count+1)))+4);
        //string = (char *)malloc(sizeof(char)*sizeof(int));
        sprintf(string, "$t0%d", count);
    }
    else{
        string = (char *)malloc(sizeof(char)*floor(log10(abs(count+1)))+4);
        //string = (char *)malloc(sizeof(char)*sizeof(int));
        sprintf(string, "$t%d", count);
    }
    
    return string;
}

Node* mergeLists(Node* list1, Node* list2){
    
    if (list1 == NULL)
        return list2;
    if (list2 == NULL)
        return list1;

    Node* nextNode = list1;
    while (nextNode != NULL)
    {
        if(nextNode->next == NULL){
            nextNode->next = list2;
            return list1;
        }
        nextNode = nextNode->next;
    }

    return list1;
    
}

void completa(Vector *v, Node* list, int line){
    char buffer[250];
    char strLine[50];
    sprintf(strLine, "%d \n", line);
    
    Node *iterator = list;
    while (iterator != NULL) {
        strcpy(buffer, vector_get(v, iterator->line));
        strcat(buffer, strLine);
        vector_set(v, iterator->line, buffer);
        iterator = iterator->next;
    }
}

Node* creaLlista(int line){
    Node *new_node;
    new_node = (Node*) malloc(sizeof(Node));
    new_node->next = NULL;
    new_node->line = line;
    return new_node;
}





