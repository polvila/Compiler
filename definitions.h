enum Tipus {
    enter,
    real,
    cadena,
    boolea
            };   //0: int, 1: real, 2: string, 3: boolean


typedef struct Node
{
    int line;
    struct Node *next;
}Node;

struct structTipusValor
{
    int   valEnter;
    double valReal;
    char* valCadena;
    int valBoolea;
    enum Tipus tipus;
    char* lloc;
    int literal;
    struct Node *llc;
    struct Node *llf;
    struct Node *lls;
    int quad;
    int test;
};


